module Lib
    ( compile
    ) where
import Parser (parse)
-- import Text.Show.Pretty (pPrint)
import Elab (elab)
import CodeGenHaskell (codeGenHaskell)
import System.FilePath (takeFileName, (</>), splitExtension)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist)
import CodeGenLaTeX (codeGenLaTeX)

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
processOutDir outDir = do
    dir <- canonicalizePath outDir

    -- create the directory if it doesn't exist
    createDirectoryIfMissing True dir

    return dir

outPaths :: FilePath -> FilePath -> IO (FilePath, FilePath)
outPaths name outDir = do
    dir <- processOutDir outDir
    return $ (dir </> (name ++ ".hs"), dir </> (name ++ ".tex"))

compile :: FilePath -> FilePath -> IO ()
compile file outDir = do
    resFile <- processFile file
    text <- readFile resFile

    putStrLn $ "Compiling file: " ++ resFile

    let parsed = parse text
    let elaborated = elab parsed
    let haskell = codeGenHaskell elaborated
    let latex = codeGenLaTeX elaborated

    let (fileName, _) = splitExtension $ takeFileName resFile
    (pathHaskell, pathLaTeX) <- outPaths fileName outDir

    -- let targetDir = takeDirectory pathHaskell
    -- createDirectoryIfMissing False targetDir
    
    writeFile pathHaskell haskell
    writeFile pathLaTeX latex

    putStrLn "Compiled successfully!"

    -- let 
    -- pPrint $ elaborated
    -- putStr haskell
