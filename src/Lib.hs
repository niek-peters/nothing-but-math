module Lib
    ( compile
    ) where
import Parser (parse)
import Text.Show.Pretty (pPrint)
import Elab (elab)

compile :: FilePath -> IO ()
compile path = do
    text <- readFile path

    let parsed = parse text
    let elaborated = elab parsed

    pPrint $ elaborated
