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
        "test1" `parsesTo` [TextFragment "This is text\n% This is a LaTeX comment\n", DefinitionFragment (AST [] [Declaration [] "a" (Signature Nothing (Type (Natural :| []))) [] (Unconditional (ImmediateInt 0)) []]), TextFragment "\nmore text\n", DefinitionFragment (AST [] [Declaration [] "b" (Signature Nothing (Type (Natural :| []))) [] (Unconditional (Call "a" [])) []]), TextFragment "\nAnd now we use evaluation to determine ", EvalFragment (Call "a" []), TextFragment " and ", EvalFragment (Call "b" []), TextFragment "."]
        "test2" `parsesTo` [DefinitionFragment (AST [] [Declaration [] "f" (Signature (Just (Type (Natural :| []))) (Type (Positive :| []))) ["x"] (Unconditional (Binary Mult (Call "y" []) (ImmediateInt 2))) [LocalDecl (Local ("y" :| []) (Binary Add (Call "x" []) (ImmediateInt 1)))], Declaration [] "g" (Signature (Just (Type (Integer :| [Natural]))) (Type (Rational :| []))) ["x", "a"] (Unconditional (Binary Div (Binary Pow (Call "x" []) (Call "a" [])) (ImmediateInt 2))) []])]


-- helper functions to make tests more concise
parsesTo :: String -> ParseResult -> SpecWith (Arg Expectation)
parsesTo fileName res = it ("correctly parses example program " ++ fileName) $ fileName `parsedShouldBe` res

parsedShouldBe :: String -> ParseResult -> Expectation
parsedShouldBe file = fileProcessedShouldBe ("test/Parsing/" ++ file ++ ".nbm") parse

