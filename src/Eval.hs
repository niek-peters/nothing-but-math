module Eval (eval) where

import Language.Haskell.Interpreter (runInterpreter, loadModules, setTopLevelModules, interpret, as)
import IR (IRExpr)
import Elab (elabTopLevelExpr)
import Parser (runExprParser)
import qualified Data.Map as Map

eval :: FilePath -> String -> [String] -> IO [IRExpr]
eval _ _ [] = pure []
eval lib modName frags = do
    result <- runInterpreter $ do
        loadModules [lib]
        setTopLevelModules [modName]
        let showExprs = map ("show $ " ++) frags
        mapM (`interpret` (as :: String)) showExprs 

    case result of
        Right val -> return $ map haskellToIR val
        Left err -> error $ show err

haskellToIR :: String -> IRExpr
haskellToIR haskell = elaborated
    where   nbm = haskellToNBM haskell
            parsed = runExprParser nbm
            elaborated = elabTopLevelExpr parsed Map.empty  -- there should be no identifiers left

haskellToNBM :: String -> String
haskellToNBM [] = []
haskellToNBM ('%':cs) = '/' : haskellToNBM cs
haskellToNBM (c:cs) = c : haskellToNBM cs