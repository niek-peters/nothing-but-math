module CodeGenHaskell.CodeGenHaskellSpec (spec) where

import Test.Hspec
import TestUtils (shouldBeGolden)
import CodeGenHaskell (codeGenHaskell)
import Elab (elab)
import Parser (parse)
import Data.List (intercalate)
import Lexer (tokenize)

spec :: Spec
spec = 
    describe "Sample Program Haskell code generation" $ 
        goldenTestFiles ["test1"]
        
    where   goldenTestFiles = mapM_ shouldCodeGenToGolden
   
shouldCodeGenToGolden :: String -> Spec
shouldCodeGenToGolden file = it ("generates correct Haskell code for example program " ++ file) $ shouldBeGolden ("test/CodeGenHaskell/" ++ file ++ ".nbm") f
    where   f = (\(lib, evalFrags) ->  lib ++ "\n\n-- EVAL FRAGS (not in standard compiler output) --\n\n" ++ intercalate "\n\n" evalFrags) . (`codeGenHaskell` "TestModule") . elab . parse . tokenize