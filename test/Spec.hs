import Test.Hspec (describe, hspec)

import qualified Parsing.ParsingSpec
import qualified Elab.ElabSpec
import qualified CodeGenHaskell.CodeGenHaskellSpec

main :: IO ()
main = hspec $ do
    describe "Parsing" Parsing.ParsingSpec.spec
    describe "Elaboration" Elab.ElabSpec.spec
    describe "Haskell code generation" CodeGenHaskell.CodeGenHaskellSpec.spec