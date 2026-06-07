module Elab.ElabSpec (spec) where

import Test.Hspec
import TestUtils (fileProcessedShouldBe, shouldBeGolden)
import Data.List.NonEmpty
import Elab (ElabResult, elab)
import Parser (parse)
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = 
    describe "Sample Program Elaboration" $ 
        goldenTestFiles ["test3"]
        
    where   goldenTestFiles = mapM_ shouldElabToGolden
   
shouldElabToGolden :: String -> Spec
shouldElabToGolden file = it ("correctly elaborates example program " ++ file) $ shouldBeGolden ("test/Elab/" ++ file ++ ".nbm") f
    where   f = ppShow . elab . parse

-- -- helper function to make tests more concise
-- shouldElabTo :: String -> ElabResult -> SpecWith (Arg Expectation)
-- shouldElabTo file = it ("correctly elaborates example program " ++ file) . fileProcessedShouldBe ("test/Elab/" ++ file ++ ".nbm") (elab . parse)
