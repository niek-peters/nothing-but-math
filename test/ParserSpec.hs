module ParserSpec (spec) where

import Test.Hspec
import Parser (ParseResult, parse)
import System.Directory (canonicalizePath)
-- import TestUtils (shouldBe')

spec :: Spec
spec = describe "TEST" $ do
    it "GG" $ do
        1 `shouldBe` 2
    -- describe "Expression Parsing" $ do
    -- it "does something" $ do
    --     parseFile "1" `shouldBe'` []

parseFile :: FilePath -> IO ParseResult
parseFile file = parse <$> (readFile =<< canonicalizePath file)
