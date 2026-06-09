module Eval (eval, EvalResult) where

import IR (IRExpr, IR, IREvalResult (IREvalResult))
import Elab (elabTopLevelExpr, ElabResult)
import Parser (runExprParser)
import qualified Data.Map as Map
import Types (Fragment(..))
import System.Process (readProcessWithExitCode)
import GHC.IO.Exception (ExitCode(..))
import Lexer (tokenize, runLexer)

type EvalResult = [Fragment String IR IREvalResult]

eval :: ElabResult -> FilePath -> [String] -> IO EvalResult
eval frags lib exprs = do
    results <- evalExprs lib exprs
    let irExprs = map haskellToIR results
    return $ replaceEvalFrags frags irExprs

-- NOTE: this function invokes GHC for each expression. In the future it would be better to either pre-compile the module or feed it all expressions as a list and parse the result
evalExprs :: FilePath -> [String] -> IO [String]
evalExprs _ [] = pure []
evalExprs lib frags = mapM evalExpr frags
    where   evalExpr expr = do
                (exitCode, stdout, stderr) <- readProcessWithExitCode "ghc" ["-v0", "-XTypeApplications", lib, "-e", expr] ""
                return $ case exitCode of
                    ExitSuccess -> init stdout  -- drop tailing newline
                    ExitFailure _ -> error stderr
                    
haskellToIR :: String -> IRExpr
haskellToIR haskell = elaborated
    where   nbm = haskellToNBM haskell
            tokenized = runLexer nbm
            parsed = runExprParser tokenized
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

-- getRuntimeGHCPath :: IO FilePath
-- getRuntimeGHCPath = do
--     res <- readProcess "ghc" ["--print-libdir"] ""
--     return $ init res