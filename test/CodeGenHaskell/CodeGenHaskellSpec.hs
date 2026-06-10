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

codeGenHaskellFromSource :: String -> ((String, [String]), ElabResult)
codeGenHaskellFromSource src = (codeGenHaskell elaborated "TestModule", elaborated)
    where   elaborated = elabFromSource src
