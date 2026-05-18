module Parser (Parser.parse) where

import Text.Megaparsec (Parsec, parse)
import Data.Void (Void)
import Text.Megaparsec.Char (string)

type Parser = Parsec Void String

parse :: String -> String
parse str = case Text.Megaparsec.parse wordParser "" str of
    Left e -> error $ show e
    Right r -> r

wordParser :: Parser String
wordParser = string "hello"

