module Eval (eval, EvalResult) where

import Language.Haskell.Interpreter (runInterpreter, loadModules, setTopLevelModules, interpret, as)
import IR (IRExpr, IR, IREvalResult (IREvalResult))
import Elab (elabTopLevelExpr, ElabResult)
import Parser (runExprParser)
import qualified Data.Map as Map
import Types (Fragment(..))

type EvalResult = [Fragment String IR IREvalResult]

eval :: ElabResult -> FilePath -> String -> [String] -> IO EvalResult
eval frags lib modName exprs = do
    results <- evalExprs lib modName exprs
    let irExprs = map haskellToIR results
    return $ replaceEvalFrags frags irExprs

evalExprs :: FilePath -> String -> [String] -> IO [String]
evalExprs _ _ [] = pure []
evalExprs lib modName frags = do
    result <- runInterpreter $ do
        loadModules [lib]
        setTopLevelModules [modName]
        let showExprs = map ("show $ " ++) frags
        
        -- forM showExprs $ \expr -> do
        --     -- res <- (Right <$> interpret expr (as :: String)) `catch` \err -> return (Left err)
        --     -- let tmp = interpret expr (as :: String)
        --     -- res <- try @InterpreterError tmp
        --     res <- (try $ interpret expr (as :: String)) :: InterpreterT IO (Either InterpreterError String)
        --     case res of
        --         Left (err) -> error "GG"
        --         Right (i) -> error "GG"
        --     error "GG"
        mapM (`interpret` (as :: String)) showExprs 

    case result of
        Right val -> return val
        Left err -> error $ show err

    -- return result
    -- case result of
    --     Right val -> return val
    --     Left err -> error $ show err

haskellToIR :: String -> IRExpr
haskellToIR haskell = elaborated
    where   nbm = haskellToNBM haskell
            parsed = runExprParser nbm
            elaborated = elabTopLevelExpr parsed Map.empty  -- there should be no identifiers left

haskellToNBM :: String -> String
haskellToNBM [] = []
haskellToNBM ('%':cs) = '/' : haskellToNBM cs
haskellToNBM (c:cs) = c : haskellToNBM cs

replaceEvalFrags :: ElabResult -> [IRExpr] -> EvalResult
replaceEvalFrags [] _ = []
replaceEvalFrags rs [] = map castFrag rs
replaceEvalFrags ((EvalFragment r):rs) (e:es) = (EvalFragment (IREvalResult r e)) : replaceEvalFrags rs es
replaceEvalFrags (r:rs) es = castFrag r : replaceEvalFrags rs es

castFrag :: Fragment String IR IRExpr -> Fragment String IR IREvalResult
castFrag (TextFragment t) = TextFragment t
castFrag (DefinitionFragment d) = DefinitionFragment d
castFrag (EvalFragment _) = error "LOGIC ERROR: Unhandled EvalFragment"