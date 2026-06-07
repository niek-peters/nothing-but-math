import Test.Hspec (describe, hspec)

import qualified Parsing.ParsingSpec

main :: IO ()
main = hspec $ do
    describe "Parsing" Parsing.ParsingSpec.spec