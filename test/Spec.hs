import Test.Hspec (describe, hspec)

import qualified Parsing.ParsingSpec
import qualified Elab.ElabSpec
import qualified CodeGenHaskell.CodeGenHaskellSpec
import qualified Eval.EvalSpec
import qualified CodeGenLaTeX.CodeGenLaTeXSpec

main :: IO ()
main = hspec $ do
    describe "Parsing" Parsing.ParsingSpec.spec
    describe "Elaboration" Elab.ElabSpec.spec
    describe "Haskell code generation" CodeGenHaskell.CodeGenHaskellSpec.spec
    describe "Evaluation" Eval.EvalSpec.spec
    describe "LaTeX code generation" CodeGenLaTeX.CodeGenLaTeXSpec.spec