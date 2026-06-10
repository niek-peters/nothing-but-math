module Eval.EvalSpec (spec, evalFromSource) where

import Eval (eval, EvalResult)
import TestUtils (testGolden)
import CodeGenHaskell.CodeGenHaskellSpec (codeGenHaskellFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)
import System.FilePath (splitExtension, addExtension)
import System.Directory (canonicalizePath)

spec :: Spec
spec =
    describe "Sample Program Evaluation" $ 
        testGolden "test/samples" "test/samples/results/Eval" "evaluates eval fragments correctly for example program" (\a b -> ppShow <$> evalFromSource a b)

evalFromSource :: FilePath -> String -> IO EvalResult
evalFromSource filePath src = do
    pathHaskell <- (addExtension <$> (fst <$> splitExtension <$> canonicalizePath filePath)) <*> (pure "generated.hs")
    writeFile pathHaskell lib

    eval elaborated pathHaskell evalFrags

    where   ((lib, evalFrags), elaborated) = codeGenHaskellFromSource src

-- import TestUtils (testGolden)
-- import Parser (parse)
-- import Lexer (tokenize)

-- import Test.Hspec
-- import Text.Show.Pretty (ppShow)
-- import CodeGenHaskell (codeGenHaskell)
-- import Elab (elab)
-- import Eval (eval)
-- import System.FilePath (splitExtension, addExtension)
-- import System.Directory (canonicalizePath)

-- spec :: Spec
-- spec =
--     describe "Sample Program Evaluation" $ 
--         testGolden "test/samples" "test/samples/results/Eval" "evaluates eval fragments correctly for example program" evalFromSource

-- evalFromSource :: FilePath -> String -> IO String
-- evalFromSource filePath src = do
--     let elaborated = (elab . parse . tokenize) src
--     let (lib, evalFrags) = (`codeGenHaskell` "TestModule") elaborated
--     pathHaskell <- (addExtension <$> (fst <$> splitExtension <$> canonicalizePath filePath)) <*> (pure "generated.hs")
--     writeFile pathHaskell lib

--     elaborated' <- eval elaborated pathHaskell evalFrags 
    
--     return $ ppShow elaborated'

-- shouldEvalToGolden :: String -> Spec
-- shouldEvalToGolden file = it ("evaluates eval fragments correctly for example program " ++ file) $ shouldBeGolden' filePath f
--     where   filePath = "test/Eval/" ++ file ++ ".nbm"
--             f str = do
--                 let elaborated = (elab . parse . tokenize) str
--                 let (lib, evalFrags) = (`codeGenHaskell` "TestModule") elaborated
--                 pathHaskell <- (addExtension <$> (fst <$> splitExtension <$> canonicalizePath filePath)) <*> (pure "generated.hs")
--                 writeFile pathHaskell lib

--                 elaborated' <- eval elaborated pathHaskell evalFrags 
                
--                 return $ ppShow elaborated'