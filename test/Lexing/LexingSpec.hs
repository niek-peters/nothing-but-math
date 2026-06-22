module Lexing.LexingSpec (spec, lexFromSource) where

import Lexer (tokenize, TokenizeResult)
import TestUtils (testGolden, shouldThrowInPhase)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = do
    describe "Sample Program Lexing" $ 
        testGolden "test/samples" "test/samples/results/Lexing" "correctly lexes example program" (const (\str -> ppShow <$> lexFromSource str ))
    
    describe "Unhappy Path Lexing" $ do
        it ("throws an error when invalid characters are encountered") $ do
            shouldThrowInPhase "test/Lexing/invalid_character.nbm" pure phase

lexFromSource :: String -> IO TokenizeResult
lexFromSource = phase

phase :: String -> IO TokenizeResult
phase = pure . tokenize

