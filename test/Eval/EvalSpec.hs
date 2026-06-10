module Eval.EvalSpec (spec, evalFromSource) where

import Eval (eval, EvalResult)
import TestUtils (testGolden)
import CodeGenHaskell.CodeGenHaskellSpec (codeGenHaskellFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec =
    describe "Sample Program Evaluation" $ 
        testGolden "test/samples" "test/samples/results/Eval" "evaluates eval fragments correctly for example program" (\a b -> ppShow <$> evalFromSource a b)

evalFromSource :: FilePath -> String -> IO EvalResult
evalFromSource filePath src = do
    writeFile filePath lib

    eval elaborated filePath evalFrags

    where   ((lib, evalFrags), elaborated) = codeGenHaskellFromSource src