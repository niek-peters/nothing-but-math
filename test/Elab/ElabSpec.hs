module Elab.ElabSpec (spec) where

import Test.Hspec
import TestUtils (fileProcessedShouldBe)
import Data.List.NonEmpty
import Elab (ElabResult, elab)
import Parser (parse)

spec :: Spec
spec = 
    describe "Sample Program Elaboration" $ do
        "test3" `shouldElabTo` []

-- helper function to make tests more concise
shouldElabTo :: String -> ElabResult -> SpecWith (Arg Expectation)
shouldElabTo file = it ("correctly elaborates example program " ++ file) . fileProcessedShouldBe ("test/Elab/" ++ file ++ ".nbm") (elab . parse)
