module Lexing.LexingSpec (spec, lexFromSource) where

import Lexer (tokenize, TokenizeResult)
import TestUtils (testGolden, shouldThrowInPhase, apply2)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = do
    describe "Sample Program Lexing" $ 
        testGolden "test/samples" "test/samples/results/Lexing" "correctly lexes example program" (ppShow `apply2` lexFromSource)
    
    describe "Unhappy Path Lexing" $ do
        it ("throws an error when invalid characters are encountered") $ do
            shouldThrowInPhase "test/Lexing/invalid_character.nbm" (const pure) phase

lexFromSource :: FilePath -> String -> IO TokenizeResult
lexFromSource = phase

phase :: FilePath -> String -> IO TokenizeResult
phase = const $ pure . tokenize

