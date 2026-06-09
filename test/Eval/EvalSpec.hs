module Eval.EvalSpec (spec) where

import Test.Hspec
import TestUtils (shouldBeGolden')
import System.Directory (canonicalizePath)
import Eval (eval)
import CodeGenHaskell (codeGenHaskell)
import Elab (elab)
import Parser (parse)
import Text.Show.Pretty (ppShow)
import System.FilePath (addExtension, splitExtension)
import Lexer (tokenize)

spec :: Spec
spec = 
    describe "Sample Program evaluation" $ 
        goldenTestFiles ["test1"]
        
    where   goldenTestFiles = mapM_ shouldEvalToGolden
   
shouldEvalToGolden :: String -> Spec
shouldEvalToGolden file = it ("evaluates eval fragments correctly for example program " ++ file) $ shouldBeGolden' filePath f
    where   filePath = "test/Eval/" ++ file ++ ".nbm"
            f str = do
                let elaborated = (elab . parse . tokenize) str
                let (lib, evalFrags) = (`codeGenHaskell` "TestModule") elaborated
                pathHaskell <- (addExtension <$> (fst <$> splitExtension <$> canonicalizePath filePath)) <*> (pure "generated.hs")
                writeFile pathHaskell lib

                elaborated' <- eval elaborated pathHaskell evalFrags 
                
                return $ ppShow elaborated'