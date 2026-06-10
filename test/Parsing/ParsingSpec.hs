module Parsing.ParsingSpec (spec, parseFromSource) where

import Parser (parse, ParseResult)
import TestUtils (testGolden)
import Lexing.LexingSpec (lexFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec =
    describe "Sample Program Parsing" $ 
        testGolden "test/samples" "test/samples/results/Parsing" "correctly parses example program" (const (pure . ppShow . parseFromSource))

parseFromSource :: String -> ParseResult
parseFromSource = parse . lexFromSource