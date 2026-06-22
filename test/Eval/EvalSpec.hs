module Eval.EvalSpec (spec, evalFromSource) where

import Eval (eval, EvalResult)
import TestUtils (testGolden, shouldThrowInPhase, apply2)
import CodeGenHaskell.CodeGenHaskellSpec (codeGenHaskellFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)
import Elab (ElabResult)

spec :: Spec
spec = do
    describe "Sample Program Evaluation" $ 
        testGolden "test/samples" "test/samples/results/Eval" "evaluates eval fragments correctly for example program" (ppShow `apply2` evalFromSource)

    describe "Unhappy Path Evaluation" $ do
        it ("throws an error when a constraint is violated") $ do
            shouldThrowInPhase "test/Eval/constraint_violation.nbm" codeGenHaskellFromSource phase

evalFromSource :: FilePath -> String -> IO EvalResult
evalFromSource file src = codeGenHaskellFromSource file src >>= phase file

phase :: FilePath -> ((String, [String]), ElabResult) -> IO EvalResult
phase file ((lib, evalFrags), elaborated) = writeFile file lib >> eval elaborated file evalFrags