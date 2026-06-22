module Elab.ElabSpec (spec, elabFromSource) where

import Elab (elab, ElabResult)
import TestUtils (testGolden, shouldThrowInPhase)
import Parsing.ParsingSpec (parseFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)
import Parser (ParseResult)

spec :: Spec
spec = do
    describe "Sample Program Elaboration" $ 
        testGolden "test/samples" "test/samples/results/Elab" "correctly elaborates example program" (const (\str -> ppShow <$> elabFromSource str))

    describe "Unhappy Path Elaboration" $ do
        it ("throws an error when undefined identifiers are referenced") $ do
            shouldThrowInPhase "test/Elab/undefined.nbm" parseFromSource phase

elabFromSource :: String -> IO ElabResult
elabFromSource str = parseFromSource str >>= phase

phase :: ParseResult -> IO ElabResult
phase = pure . elab