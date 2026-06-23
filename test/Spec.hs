-- | Test suite entry point for the compiler.
--
-- The suite groups the individual compilation phase tests so the full pipeline can be run from a single `stack test`
import Test.Hspec (describe, hspec)

import qualified Lexing.LexingSpec
import qualified Parsing.ParsingSpec
import qualified Elab.ElabSpec
import qualified CodeGenHaskell.CodeGenHaskellSpec
import qualified Eval.EvalSpec
import qualified CodeGenLaTeX.CodeGenLaTeXSpec

-- Future Work:
-- - Add QuickCheck ParseResult -> pretty print -> ParseResult testing
-- - Add QuickCheck ElabResult -> pretty print -> ElabResult testing
-- - Add tests for specific critical functions

-- | Run the full test suite.
main :: IO ()
main = hspec $ do
    describe "Lexing" Lexing.LexingSpec.spec
    describe "Parsing" Parsing.ParsingSpec.spec
    describe "Elaboration" Elab.ElabSpec.spec
    describe "Haskell code generation" CodeGenHaskell.CodeGenHaskellSpec.spec
    describe "Evaluation" Eval.EvalSpec.spec
    describe "LaTeX code generation" CodeGenLaTeX.CodeGenLaTeXSpec.spec