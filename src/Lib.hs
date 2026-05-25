module Lib
    ( compile
    ) where
import Parser (parse)
-- import Text.Show.Pretty (pPrint)
import Elab (elab)
import CodeGenHaskell (codeGenHaskell)
import System.FilePath (takeFileName, (</>), splitExtension, takeDirectory)
import System.Directory (canonicalizePath, createDirectoryIfMissing)

outDir :: IO FilePath
outDir = canonicalizePath "./out"

outPathHaskell :: FilePath -> IO FilePath
outPathHaskell name = do
    dir <- outDir
    return $ dir </> (name ++ ".hs")

compile :: FilePath -> IO ()
compile path = do
    text <- readFile path

    let parsed = parse text
    let elaborated = elab parsed
    let haskell = codeGenHaskell elaborated

    let (fileName, _) = splitExtension $ takeFileName path
    pathHaskell <- outPathHaskell fileName

    let targetDir = takeDirectory pathHaskell
    createDirectoryIfMissing False targetDir
    
    writeFile pathHaskell haskell
    -- let 
    -- pPrint $ elaborated
    -- putStr haskell
