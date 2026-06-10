module Lexing.LexingSpec (spec, lexFromSource) where

import Lexer (tokenize, TokenizeResult)
import TestUtils (testGolden)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec =
    describe "Sample Program Lexing" $ 
        testGolden "test/samples" "test/samples/results/Lexing" "correctly lexes example program" (const (pure . ppShow . lexFromSource))

lexFromSource :: String -> TokenizeResult
lexFromSource = tokenize