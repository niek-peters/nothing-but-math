module Lib
    ( compile
    ) where
import Parser (parse)
import Text.Show.Pretty (pPrint)
import Elab (elab)
import CodeGenHaskell (codeGenHaskell)
import System.FilePath (takeFileName, (</>), splitExtension, replaceExtension)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, removeFile)
import CodeGenLaTeX (codeGenLaTeX)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Monad (when)
import Types (CLIOptions (..))

compile :: CLIOptions -> IO ()
compile options = do
    resFile <- processFile $ filePath options
    text <- readFile resFile

    putStrLn $ "Compiling file: " ++ resFile

    let parsed = parse text
    let elaborated = elab parsed
    let haskell = codeGenHaskell elaborated
    let tmpLatex = codeGenLaTeX elaborated
    let latex = case wrapDoc options of
            True -> "\\documentclass{article}\n\\usepackage{amsmath, amssymb}\n\n\\begin{document}\n\n" ++ tmpLatex ++ "\n\n\\end{document}"
            False -> tmpLatex

    let (fileName, _) = splitExtension $ takeFileName resFile
    (pathHaskell, pathLaTeX, dir) <- outPaths fileName $ outDir options

    -- let targetDir = takeDirectory pathHaskell
    -- createDirectoryIfMissing False targetDir
    
    writeFile pathHaskell haskell
    writeFile pathLaTeX latex

    putStrLn "NBM Compiled successfully!"

    when (toPDF options) (compileToPDF pathLaTeX dir)

    -- if toPDF then do compileToPDF pathLaTeX dir
    -- else return () 

    -- let 
    -- pPrint $ elaborated
    -- putStr haskell


processFile :: FilePath -> IO FilePath
processFile file = do
    let (_, ext) = splitExtension $ takeFileName file
    exists <- doesFileExist file

    let resFile = case exists of
            True -> file
            False -> case ext of
                "nbm" -> file
                _ -> file ++ ".nbm"

    return resFile

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
    (exitCode, stdout, _) <- readProcessWithExitCode "pdflatex" ["-interaction=nonstopmode", "-output-directory=" ++ dir, texFile] ""

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
    let (auxFile, logFile) = (replace "aux", replace "log")

    removeIfExists auxFile
    removeIfExists logFile

removeIfExists :: FilePath -> IO ()
removeIfExists file = do
    exists <- doesFileExist file
    when exists (removeFile file)