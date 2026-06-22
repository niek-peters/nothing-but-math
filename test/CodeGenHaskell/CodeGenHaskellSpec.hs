module CodeGenHaskell.CodeGenHaskellSpec (spec, codeGenHaskellFromSource) where

import CodeGenHaskell (codeGenHaskell)
import TestUtils (testGolden, apply2)
import Elab.ElabSpec (elabFromSource)

import Test.Hspec
import Data.List (intercalate)
import Elab (ElabResult)

spec :: Spec
spec = do
    describe "Sample Program Haskell code generation" $ 
        testGolden "test/samples" "test/samples/results/CodeGenHaskell" "generates correct Haskell code for example program" (toTestStr `apply2` codeGenHaskellFromSource)
    where   toTestStr = (\(lib, evalFrags) -> lib ++ "\n\n-- EVAL FRAGS (not in standard compiler output) --\n\n" ++ intercalate "\n\n" evalFrags) . fst

codeGenHaskellFromSource :: FilePath -> String -> IO ((String, [String]), ElabResult)
codeGenHaskellFromSource file str = elabFromSource file str >>= phase file

phase :: FilePath -> ElabResult -> IO ((String, [String]), ElabResult)
phase _ e = pure (codeGenHaskell e "TestModule", e)