module Parsing.ParsingSpec (spec) where

import Test.Hspec
import Parser (parse)
import TestUtils (shouldBeGolden)
import Text.Show.Pretty (ppShow)
import Lexer (tokenize)

spec :: Spec
spec = 
    describe "Sample Program Parsing" $
        goldenTestFiles ["test1", "test2", "test3"]
    where   goldenTestFiles = mapM_ shouldParseToGolden

shouldParseToGolden :: String -> Spec
shouldParseToGolden file = it ("correctly parses example program " ++ file) $ shouldBeGolden ("test/Parsing/" ++ file ++ ".nbm") f 
    where   f = ppShow . parse . tokenize