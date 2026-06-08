module Lib
    ( compile
    ) where

import Parser (parse)
import Elab (elab)
import CodeGenHaskell (codeGenHaskell)
import Eval (eval)
import System.FilePath (takeFileName, (</>), splitExtension, replaceExtension)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, removeFile)
import CodeGenLaTeX (codeGenLaTeX)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Monad (when)
import Types (CLIOptions (..))
-- import Text.Show.Pretty (pPrint)

compile :: CLIOptions -> IO ()
compile options = do
    file <- resolveFilePath $ filePath options
    text <- readFile file

    putStrLn $ "Compiling file: " ++ file

    let (fileName, _) = splitExtension $ takeFileName file
    (pathHaskell, pathLaTeX, dir) <- outPaths fileName $ outDir options

    let parsed = parse text
    let elaborated = elab parsed
    -- pPrint elaborated
    let (lib, evalFrags) = codeGenHaskell elaborated $ moduleName options

    writeFile pathHaskell lib
    
    elaborated' <- eval elaborated pathHaskell evalFrags 

    let tmpLatex = codeGenLaTeX elaborated'
    let latex = case wrapDoc options of
            True -> "\\documentclass{article}\n\\usepackage{amsmath, amssymb, hyperref}\n\n\\begin{document}\n\n" ++ tmpLatex ++ "\n\n\\end{document}"
            False -> tmpLatex
    
    writeFile pathLaTeX latex

    putStrLn "NBM Compiled successfully!"

    when (toPDF options) (compileToPDF pathLaTeX dir)

resolveFilePath :: FilePath -> IO FilePath
resolveFilePath file = do
    let (_, ext) = splitExtension $ takeFileName file
    exists <- doesFileExist file

    let resFile = case exists of
            True -> file
            False -> case ext of
                "nbm" -> file
                _ -> file ++ ".nbm"

    canonicalizePath resFile

processOutDir :: FilePath -> IO FilePath
processOutDir out = do
    dir <- canonicalizePath out

    -- create the directory if it doesn't exist
    createDirectoryIfMissing True dir

    return dir

outPaths :: FilePath -> FilePath -> IO (FilePath, FilePath, FilePath)
outPaths name out = do
    dir <- processOutDir out
    return $ (dir </> (name ++ ".hs"), dir </> (name ++ ".tex"), dir)

compileToPDF :: FilePath -> FilePath -> IO ()
compileToPDF texFile dir = do
    let options = ["-interaction=nonstopmode", "-output-directory=" ++ dir, texFile]
    
    -- we run pdflatex twice to resolve references
    _ <- readProcessWithExitCode "pdflatex" options ""
    (exitCode, stdout, _) <- readProcessWithExitCode "pdflatex" options ""

    case exitCode of
        ExitSuccess -> do
            putStrLn "PDF generation successful!"
            cleanUpPDFArtifacts texFile
        ExitFailure _ -> do
            putStrLn "LaTeX compilation with pdflatex failed"
            mapM_ putStrLn (lines stdout)

cleanUpPDFArtifacts :: FilePath -> IO ()
cleanUpPDFArtifacts texFile = do
    let replace = replaceExtension texFile
    let (auxFile, logFile, outFile) = (replace "aux", replace "log", replace "out")

    removeIfExists auxFile
    removeIfExists logFile
    removeIfExists outFile

removeIfExists :: FilePath -> IO ()
removeIfExists file = do
    exists <- doesFileExist file
    when exists (removeFile file)