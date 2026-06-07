module Parsing.ParsingSpec (spec) where

import Test.Hspec
import Parser (ParseResult, parse)
import System.Directory (canonicalizePath)
import TestUtils (shouldBe', fileProcessedShouldBe)
import Types
import AST
import Data.List.NonEmpty

spec :: Spec
spec = 
    -- describe "TEST" $ do
    -- it "GG" $ do
    --     1 `shouldBe` 2
    describe "Expression Parsing" $ do
        it "does something" $ do
            "test1" `parsedShouldBe` [DefinitionFragment (AST [] [Declaration [] "a" (Signature Nothing (Type (Natural :| []))) [] (Unconditional (ImmediateInt 0)) []])]

parsedShouldBe :: String -> ParseResult -> Expectation
parsedShouldBe file = fileProcessedShouldBe ("test/Parsing/" ++ file ++ ".nbm") parse

-- parsedShouldBe :: String -> ParseResult -> IO ()
-- parsedShouldBe file val = parseFile file >>= (`shouldBe` val)

-- parseFile :: FilePath -> IO ParseResult
-- parseFile file = parse <$> (readFile =<< canonicalizePath ("test/Parsing/" ++ file ++ ".nbm"))

