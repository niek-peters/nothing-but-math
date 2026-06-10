module Elab.ElabSpec (spec, elabFromSource) where

import Elab (elab, ElabResult)
import TestUtils (testGolden)
import Parsing.ParsingSpec (parseFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec =
    describe "Sample Program Elaboration" $ 
        testGolden "test/samples" "test/samples/results/Elab" "correctly elaborates example program" (const (pure . ppShow . elabFromSource))

elabFromSource :: String -> ElabResult
elabFromSource = elab . parseFromSource