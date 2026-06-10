import Test.Hspec (describe, hspec)

import qualified Lexing.LexingSpec
import qualified Parsing.ParsingSpec
import qualified Elab.ElabSpec
import qualified CodeGenHaskell.CodeGenHaskellSpec
import qualified Eval.EvalSpec
import qualified CodeGenLaTeX.CodeGenLaTeXSpec

-- TODO:
-- - Add non-happy path tests (compile-time AND runtime errors)
-- - Make tests run in a chain to avoid redoing work (maybe this is stupid because parallelization)
-- - Write way more golden tests
-- - Actually check all the generated outputs for correctness

-- Probably out of time/scope:
-- - Add QuickCheck ParseResult -> pretty print -> ParseResult testing
-- - Add QuickCheck ElabResult -> pretty print -> ElabResult testing
-- - Add tests for specific critical functions

main :: IO ()
main = hspec $ do
    describe "Lexing" Lexing.LexingSpec.spec
    describe "Parsing" Parsing.ParsingSpec.spec
    describe "Elaboration" Elab.ElabSpec.spec
    describe "Haskell code generation" CodeGenHaskell.CodeGenHaskellSpec.spec
    describe "Evaluation" Eval.EvalSpec.spec
    describe "LaTeX code generation" CodeGenLaTeX.CodeGenLaTeXSpec.spec