module Lib
    ( compile
    ) where
import Parser (parse)
import Text.Show.Pretty (pPrint)
import Elab (collectGlobals)

compile :: FilePath -> IO ()
compile path = do
    text <- readFile path

    let parsed = parse text
    pPrint $ collectGlobals parsed
