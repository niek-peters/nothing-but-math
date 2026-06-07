module CodeGenHaskell.CodeGenHaskellSpec (spec) where

import Test.Hspec
import TestUtils (fileProcessedShouldBe, shouldBeGolden)
import Data.List.NonEmpty
import CodeGenHaskell (codeGenHaskell)
import Elab (ElabResult, elab)
import Parser (parse)
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = 
    describe "Sample Program Haskell code generation" $ 
        goldenTestFiles ["test1"]
        
    where   goldenTestFiles = mapM_ shouldElabToGolden
   
shouldElabToGolden :: String -> Spec
shouldElabToGolden file = it ("generates correct Haskell code for example program " ++ file) $ shouldBeGolden ("test/CodeGenHaskell/" ++ file ++ ".nbm") f
    where   f = ppShow . (`codeGenHaskell` "TestModule") . elab . parse