module CodeGenLaTeX.CodeGenLaTeXSpec (spec) where

import CodeGenLaTeX (codeGenLaTeX)
import TestUtils (testGolden)

import Test.Hspec
import Eval.EvalSpec (evalFromSource)
import Eval (EvalResult)

spec :: Spec
spec =
    describe "Sample Program LaTeX code generation" $ 
        testGolden "test/samples" "test/samples/results/CodeGenLaTeX" "generates correct LaTeX code for example program" codeGenLaTeXFromSource

codeGenLaTeXFromSource :: FilePath -> String -> IO String
codeGenLaTeXFromSource file src = evalFromSource file src >>= phase

phase :: EvalResult -> IO String
phase = pure . codeGenLaTeX