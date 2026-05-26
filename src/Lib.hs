module Lib
    ( compile
    ) where
import Parser (parse)
-- import Text.Show.Pretty (pPrint)
import Elab (elab)
import CodeGenHaskell (codeGenHaskell)
import System.FilePath (takeFileName, (</>), splitExtension, takeDirectory)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import CodeGenLaTeX (codeGenLaTeX)

outDir :: IO FilePath
outDir = do
    dir <- canonicalizePath "./out"

    -- create the directory if it doesn't exist
    createDirectoryIfMissing False dir

    return dir

outPaths :: FilePath -> IO (FilePath, FilePath)
outPaths name = do
    dir <- outDir
    return $ (dir </> (name ++ ".hs"), dir </> (name ++ ".tex"))

compile :: FilePath -> IO ()
compile path = do
    text <- readFile path

    let parsed = parse text
    let elaborated = elab parsed
    let haskell = codeGenHaskell elaborated
    let latex = codeGenLaTeX elaborated

    let (fileName, _) = splitExtension $ takeFileName path
    (pathHaskell, pathLaTeX) <- outPaths fileName

    -- let targetDir = takeDirectory pathHaskell
    -- createDirectoryIfMissing False targetDir
    
    writeFile pathHaskell haskell
    writeFile pathLaTeX latex
    -- let 
    -- pPrint $ elaborated
    -- putStr haskell
