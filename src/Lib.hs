module Lib
    ( compile
    ) where
import Parser (parse, runSectionsParser)
import Text.Show.Pretty (pPrint)

compile :: FilePath -> IO ()
compile path = do
    text <- readFile path

    let parsed = parse text
    pPrint parsed
