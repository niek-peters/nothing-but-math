module CodeGenLaTeX.CodeGenLaTeXSpec (spec) where

import Test.Hspec
import TestUtils (shouldBeGolden')
import CodeGenHaskell (codeGenHaskell)
import Elab (elab)
import Parser (parse)
import System.FilePath (addExtension, splitExtension)
import System.Directory (canonicalizePath)
import Eval (eval)
import CodeGenLaTeX (codeGenLaTeX)

spec :: Spec
spec = 
    describe "Sample Program LaTeX code generation" $ 
        goldenTestFiles ["test1"]
        
    where   goldenTestFiles = mapM_ shouldCodeGenToGolden
   
shouldCodeGenToGolden :: String -> Spec
shouldCodeGenToGolden file = it ("generates correct LaTeX code for example program " ++ file) $ shouldBeGolden' filePath f
    where   filePath = "test/CodeGenLaTeX/" ++ file ++ ".nbm"
            f str = do
                let elaborated = (elab . parse) str
                let (lib, evalFrags) = (`codeGenHaskell` "TestModule") elaborated
                pathHaskell <- (addExtension <$> (fst <$> splitExtension <$> canonicalizePath filePath)) <*> (pure "generated.hs")
                writeFile pathHaskell lib

                elaborated' <- eval elaborated pathHaskell evalFrags 
                
                return $ codeGenLaTeX elaborated'