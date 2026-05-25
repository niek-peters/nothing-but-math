module Lib
    ( compile
    ) where
import Parser (parse)
import Text.Show.Pretty (pPrint)
import Elab (elab)
import CodeGenHaskell (codeGenHaskell)

compile :: FilePath -> IO ()
compile path = do
    text <- readFile path

    let parsed = parse text
    let elaborated = elab parsed
    let haskell = codeGenHaskell elaborated

    -- pPrint $ elaborated
    putStr haskell
