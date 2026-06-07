module Main (main) where
    
import Test.Hspec (describe, hspec)

import qualified ParserSpec

main :: IO ()
main = hspec $ do
    describe "Parsing" ParserSpec.spec