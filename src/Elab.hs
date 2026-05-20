module Elab where
import Parser (ParseResult(..), Fragment (..))
import qualified Data.Map as Map
import AST (Id, Signature, AST(..), Declaration (..))

-- From the design of the DSL we get some unique benefits:
-- 1. There are only 2 levels of scope: global and local
-- 2. All global scope declarations have explicit type signatures
--
-- Now, to simplify type checking, we can split the elaboration phase into 2 phases:
-- 1. Collect the global scope
-- 2. Type check the internals of every declaration, already knowing the global scope

collectGlobals :: ParseResult -> Map.Map Id Signature
collectGlobals (ParseResult frags) = foldl collect Map.empty frags
    where   collect m (TextFragment _) = m
            collect m (CodeFragment ast) = collectGlobalsFromAST m ast

collectGlobalsFromAST :: Map.Map Id Signature -> AST -> Map.Map Id Signature
collectGlobalsFromAST m (AST decls) = foldl insertDeclaration m decls

insertDeclaration :: Map.Map Id Signature -> Declaration -> Map.Map Id Signature
insertDeclaration m (Declaration ident sig _ _ _ _) 
    | Map.member ident m = error $ "Duplicate declaration of '" ++ show ident ++ "'"
    | otherwise = Map.insert ident sig m