module Elab.ElabSpec (spec) where

import Test.Hspec
import TestUtils (shouldBeGolden)
import Elab (elab)
import Parser (parse)
import Text.Show.Pretty (ppShow)
import Lexer (tokenize)

spec :: Spec
spec = 
    describe "Sample Program Elaboration" $ 
        goldenTestFiles ["test3"]
        
    where   goldenTestFiles = mapM_ shouldElabToGolden
   
shouldElabToGolden :: String -> Spec
shouldElabToGolden file = it ("correctly elaborates example program " ++ file) $ shouldBeGolden ("test/Elab/" ++ file ++ ".nbm") f
    where   f = ppShow . elab . parse . tokenize