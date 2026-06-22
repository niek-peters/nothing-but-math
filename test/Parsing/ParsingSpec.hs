module Parsing.ParsingSpec (spec, parseFromSource) where

import Parser (parse, ParseResult)
import TestUtils (testGolden, shouldThrowInPhase)
import Lexing.LexingSpec (lexFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)
import Lexer (TokenizeResult)

spec :: Spec
spec = do
    describe "Sample Program Parsing" $ 
        testGolden "test/samples" "test/samples/results/Parsing" "correctly parses example program" (const (\str -> ppShow <$> parseFromSource str))

    describe "Unhappy Path Parsing" $ do
        it ("throws an error when a definition is missing a signature") $ do
            shouldThrowInPhase "test/Parsing/no_signature.nbm" lexFromSource phase
            shouldThrowInPhase "test/Parsing/text_in_code.nbm" lexFromSource phase

parseFromSource :: String -> IO ParseResult
parseFromSource str = lexFromSource str >>= phase

phase :: TokenizeResult -> IO ParseResult
phase = pure . parse