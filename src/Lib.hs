module Lib
    ( compile
    ) where
import Parser (parse)

compile :: FilePath -> IO ()
compile path = do
    text <- readFile path

    let parsed = parse text
    putStr parsed
