module CodeGenHaskell.CodeGenHaskellSpec (spec, codeGenHaskellFromSource) where

import CodeGenHaskell (codeGenHaskell)
import TestUtils (testGolden)
import Elab.ElabSpec (elabFromSource)

import Test.Hspec
import Data.List (intercalate)
import Elab (ElabResult)

spec :: Spec
spec =
    describe "Sample Program Haskell code generation" $ 
        testGolden "test/samples" "test/samples/results/CodeGenHaskell" "generates correct Haskell code for example program" (const (pure . toTestStr . codeGenHaskellFromSource))
    where   toTestStr = (\(lib, evalFrags) -> lib ++ "\n\n-- EVAL FRAGS (not in standard compiler output) --\n\n" ++ intercalate "\n\n" evalFrags) . fst
-- codeGenHaskellFromSource :: String -> String
-- codeGenHaskellFromSource = fst . gen

codeGenHaskellFromSource :: String -> ((String, [String]), ElabResult)
codeGenHaskellFromSource src = (codeGenHaskell elaborated "TestModule", elaborated)
    where   elaborated = elabFromSource src

-- import Test.Hspec
-- import TestUtils (shouldBeGolden)
-- import CodeGenHaskell (codeGenHaskell)
-- import Elab (elab)
-- import Parser (parse)
-- import Data.List (intercalate)
-- import Lexer (tokenize)

-- spec :: Spec
-- spec = 
--     describe "Sample Program Haskell code generation" $ 
--         goldenTestFiles ["test1"]
        
--     where   goldenTestFiles = mapM_ shouldCodeGenToGolden
   
-- shouldCodeGenToGolden :: String -> Spec
-- shouldCodeGenToGolden file = it ("generates correct Haskell code for example program " ++ file) $ shouldBeGolden ("test/CodeGenHaskell/" ++ file ++ ".nbm") f
--     where   f = (\(lib, evalFrags) ->  lib ++ "\n\n-- EVAL FRAGS (not in standard compiler output) --\n\n" ++ intercalate "\n\n" evalFrags) . (`codeGenHaskell` "TestModule") . elab . parse . tokenize