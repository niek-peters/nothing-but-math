module Elab.ElabSpec (spec, elabFromSource) where

import Elab (elab, ElabResult)
import TestUtils (testGolden, shouldThrowInPhase, apply2)
import Parsing.ParsingSpec (parseFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)
import Parser (ParseResult)

spec :: Spec
spec = do
    describe "Sample Program Elaboration" $ 
        testGolden "test/samples" "test/samples/results/Elab" "correctly elaborates example program" (ppShow `apply2` elabFromSource)

    describe "Unhappy Path Elaboration" $ do
        it ("throws an error when undefined identifiers are referenced") $ do
            shouldThrowInPhase "test/Elab/undefined.nbm" parseFromSource phase
        it ("throws an error when an illegal cast is encountered") $ do
            shouldThrowInPhase "test/Elab/illegal_cast.nbm" parseFromSource phase

elabFromSource :: FilePath -> String -> IO ElabResult
elabFromSource file str = parseFromSource file str >>= phase file

phase :: FilePath -> ParseResult -> IO ElabResult
phase = const $ pure . elab