import Test.Hspec (describe, hspec)

import qualified Parsing.ParsingSpec
import qualified Elab.ElabSpec

main :: IO ()
main = hspec $ do
    describe "Parsing" Parsing.ParsingSpec.spec
    describe "Elaboration" Elab.ElabSpec.spec