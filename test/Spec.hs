import Test.Hspec (describe, hspec)

import qualified Parsing.ParsingSpec
import qualified Elab.ElabSpec
import qualified CodeGenHaskell.CodeGenHaskellSpec
import qualified Eval.EvalSpec
import qualified CodeGenLaTeX.CodeGenLaTeXSpec

-- TODO:
-- - Write way more golden tests
-- - Add QuickCheck ParseResult -> pretty print -> ParseResult testing
-- - Add QuickCheck ElabResult -> pretty print -> ElabResult testing
-- - Add tests for specific critical functions

main :: IO ()
main = hspec $ do
    describe "Parsing" Parsing.ParsingSpec.spec
    describe "Elaboration" Elab.ElabSpec.spec
    describe "Haskell code generation" CodeGenHaskell.CodeGenHaskellSpec.spec
    describe "Evaluation" Eval.EvalSpec.spec
    describe "LaTeX code generation" CodeGenLaTeX.CodeGenLaTeXSpec.spec