module Parsing.ParsingSpec (spec, parseFromSource) where

import Parser (parse, ParseResult)
import TestUtils (testGolden, shouldThrowInPhase, apply2)
import Lexing.LexingSpec (lexFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)
import Lexer (TokenizeResult)

spec :: Spec
spec = do
    describe "Sample Program Parsing" $ 
        testGolden "test/samples" "test/samples/results/Parsing" "correctly parses example program" (ppShow `apply2` parseFromSource)

    describe "Unhappy Path Parsing" $ do
        it ("throws an error when a definition is missing a signature") $ do
            shouldThrowInPhase "test/Parsing/no_signature.nbm" lexFromSource phase
            shouldThrowInPhase "test/Parsing/text_in_code.nbm" lexFromSource phase

parseFromSource :: FilePath -> String -> IO ParseResult
parseFromSource file str = lexFromSource file str >>= phase file

phase :: FilePath -> TokenizeResult -> IO ParseResult
phase = const $ pure . parse