module CodeGenLaTeX.CodeGenLaTeXSpec (spec) where

import CodeGenLaTeX (codeGenLaTeX)
import TestUtils (testGolden)
import CodeGenHaskell.CodeGenHaskellSpec (codeGenHaskellFromSource)

import Test.Hspec
import Text.Show.Pretty (ppShow)
import System.FilePath (splitExtension, addExtension)
import System.Directory (canonicalizePath)
import Eval.EvalSpec (evalFromSource)

spec :: Spec
spec =
    describe "Sample Program LaTeX code generation" $ 
        testGolden "test/samples" "test/samples/results/CodeGenLaTeX" "generates correct LaTeX code for example program" codeGenLaTeXFromSource

codeGenLaTeXFromSource :: FilePath -> String -> IO String
codeGenLaTeXFromSource file src = codeGenLaTeX <$> evalFromSource file src

-- import Test.Hspec
-- import TestUtils (shouldBeGolden')
-- import CodeGenHaskell (codeGenHaskell)
-- import Elab (elab)
-- import Parser (parse)
-- import System.FilePath (addExtension, splitExtension)
-- import System.Directory (canonicalizePath)
-- import Eval (eval)
-- import CodeGenLaTeX (codeGenLaTeX)
-- import Lexer (tokenize)

-- spec :: Spec
-- spec = 
--     describe "Sample Program LaTeX code generation" $ 
--         goldenTestFiles ["test1"]
        
--     where   goldenTestFiles = mapM_ shouldCodeGenToGolden
   
-- shouldCodeGenToGolden :: String -> Spec
-- shouldCodeGenToGolden file = it ("generates correct LaTeX code for example program " ++ file) $ shouldBeGolden' filePath f
--     where   filePath = "test/CodeGenLaTeX/" ++ file ++ ".nbm"
--             f str = do
--                 let elaborated = (elab . parse . tokenize) str
--                 let (lib, evalFrags) = (`codeGenHaskell` "TestModule") elaborated
--                 pathHaskell <- (addExtension <$> (fst <$> splitExtension <$> canonicalizePath filePath)) <*> (pure "generated.hs")
--                 writeFile pathHaskell lib

--                 elaborated' <- eval elaborated pathHaskell evalFrags 
                
--                 return $ codeGenLaTeX elaborated'