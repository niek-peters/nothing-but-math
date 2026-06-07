module Parsing.ParsingSpec (spec) where

import Test.Hspec
import Parser (ParseResult, parse)
import TestUtils (fileProcessedShouldBe)
import Types
import AST
import Data.List.NonEmpty

spec :: Spec
spec = 
    describe "File Parsing" $ do
        it "correctly parses example program test1" $ "test1" `parsedShouldBe` [DefinitionFragment (AST [] [Declaration [] "a" (Signature Nothing (Type (Natural :| []))) [] (Unconditional (ImmediateInt 0)) []])]

parsedShouldBe :: String -> ParseResult -> Expectation
parsedShouldBe file = fileProcessedShouldBe ("test/Parsing/" ++ file ++ ".nbm") parse

