module CodeGenHaskell.CodeGenHaskellSpec (spec, codeGenHaskellFromSource) where

import CodeGenHaskell (codeGenHaskell)
import TestUtils (testGolden, shouldThrowInPhase)
import Elab.ElabSpec (elabFromSource)

import Test.Hspec
import Data.List (intercalate)
import Elab (ElabResult)

spec :: Spec
spec = do
    describe "Sample Program Haskell code generation" $ 
        testGolden "test/samples" "test/samples/results/CodeGenHaskell" "generates correct Haskell code for example program" (const (\str -> toTestStr <$> codeGenHaskellFromSource str))


    -- describe "Unhappy Path Elaboration" $ do
    --     it ("throws an error when undefined identifiers are referenced") $ do
    --         shouldThrowInPhase "test/Elab/undefined.nbm" elabPrep elabPhase
    
    where   toTestStr = (\(lib, evalFrags) -> lib ++ "\n\n-- EVAL FRAGS (not in standard compiler output) --\n\n" ++ intercalate "\n\n" evalFrags) . fst

codeGenHaskellFromSource :: String -> IO ((String, [String]), ElabResult)
codeGenHaskellFromSource str = elabFromSource str >>= phase
-- codeGenHaskellFromSource src = (codeGenHaskell elaborated "TestModule", elaborated)
--     where   elaborated = elabFromSource src

-- codeGenHaskellPrep :: String -> IO ElabResult
-- codeGenHaskellPrep = pure . elabFromSource

phase :: ElabResult -> IO ((String, [String]), ElabResult)
phase e = pure (codeGenHaskell e "TestModule", e)
-- codeGenHaskllPhase = 