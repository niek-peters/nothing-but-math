module Parsing.ParsingSpec (spec, parseFromSource) where

import Parser (parse, ParseResult)
import TestUtils (testGolden, shouldThrowInPhase)
import Lexing.LexingSpec (lexFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = do
    describe "Sample Program Parsing" $ 
        testGolden "test/samples" "test/samples/results/Parsing" "correctly parses example program" (const (pure . ppShow . parseFromSource))
    describe "Unhappy Path Parsing" $ do
        it ("throws an error when a definition is missing a signature") $ 
            shouldThrowInPhase "test/Parsing/no_signature.nbm" (pure . lexFromSource) (pure . parse)

parseFromSource :: String -> ParseResult
parseFromSource = parse . lexFromSource