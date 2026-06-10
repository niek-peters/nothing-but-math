module Elab.ElabSpec (spec, elabFromSource) where

import Elab (elab, ElabResult)
import TestUtils (testGolden)
import Parsing.ParsingSpec (parseFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec =
    describe "Sample Program Elaboration" $ 
        testGolden "test/samples" "test/samples/results/Parsing" "correctly parses example program" (const (pure . ppShow . elabFromSource))

elabFromSource :: String -> ElabResult
elabFromSource = elab . parseFromSource

-- spec :: Spec
-- spec = 
--     describe "Sample Program Elaboration" $ 
--         goldenTestFiles ["test3"]
        
--     where   goldenTestFiles = mapM_ shouldElabToGolden
   
-- shouldElabToGolden :: String -> Spec
-- shouldElabToGolden file = it ("correctly elaborates example program " ++ file) $ shouldBeGolden ("test/Elab/" ++ file ++ ".nbm") f
--     where   f = ppShow . elab . parse . tokenize