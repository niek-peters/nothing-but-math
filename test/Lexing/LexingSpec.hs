module Lexing.LexingSpec (spec, lexFromSource) where

import Lexer (tokenize, TokenizeResult)
import TestUtils (testGolden, shouldThrowInPhase)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = do
    describe "Sample Program Lexing" $ 
        testGolden "test/samples" "test/samples/results/Lexing" "correctly lexes example program" (const (pure . ppShow . lexFromSource))
    
    describe "Unhappy Path Lexing" $ do
        it ("throws an error when invalid characters are encountered") $ 
            shouldThrowInPhase "test/Lexing/invalid_character.nbm" (pure . id) (pure . lexFromSource)
        --thingy "test/Lexing/invalid_character.nbm" (pure . lexFromSource)
            -- src <- readFile ""
            
            -- shouldThrow  (error src) anyErrorCall

-- shouldThrowInPhase :: (Show b) => String -> (String -> IO a) -> (a -> IO b) -> Expectation
-- shouldThrowInPhase name prep final = do
--     file <- canonicalizePath name
--     src <- readFile file
--     tmp <- prep src -- it should not throw an error in previous compiler phases

--     -- then it should throw in the final phase
--     shouldThrow (final tmp >>= print) anyErrorCall  -- print forces evaluation (but nothing will be printed if there is an error as we expect)



-- thingy :: (Show a) => String -> (String -> IO a) -> Expectation
-- thingy str f = do
--     src <- readFile str

--     shouldThrow (f src >>= print) anyErrorCall  -- print forces evaluation (but nothing will be printed if there is an error as we expect)

lexFromSource :: String -> TokenizeResult
lexFromSource = tokenize