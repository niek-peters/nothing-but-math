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
-- 2. Elaborate the internals of every declaration, already knowing the global scope

-- Now, implementing static type *checking* seems to be too difficult, as it would require a complete recursive top-down type inference approach, starting at the function declaration,
-- that then applies constraints to every expression it encounters.
-- Then, from the leaves in a bottom-up type synthesis approach, it checks whether those constraints can be fulfilled, and resolve to the 'smallest' type possible of those constraints

-- An alternative would be to rely solely on a bottom-up type synthesis approach, with knowledge of the types of any references
-- This would also use widening of types if we know an operation could throw a runtime error (e.g. ((a :: Natural) - (b :: Natural)) would instead become ((a :: Integer) - (b :: Integer)))
-- The problem here is that we mostly lose type checking on numbers, instead doing runtime implicit narrowing of types whenever there is a mismatch between the synthesized and required type
-- You could also make casts explicit, but that makes the DSL significantly harder to use

-- So, we will not be having compile-time type errors for mismatches in number types, instead inserting many type casts automatically and trusting the developer that the given type signature is correct

type GlobalScope = Map.Map Id Signature

collectGlobals :: ParseResult -> GlobalScope
collectGlobals (ParseResult frags) = foldl collect Map.empty frags
    where   collect m (TextFragment _) = m
            collect m (CodeFragment ast) = collectGlobalsFromAST m ast

collectGlobalsFromAST :: GlobalScope -> AST -> GlobalScope
collectGlobalsFromAST m (AST decls) = foldl insertDeclaration m decls

insertDeclaration :: GlobalScope -> Declaration -> GlobalScope
insertDeclaration m (Declaration ident sig _ _ _ _) 
    | Map.member ident m = error $ "Duplicate declaration of '" ++ show ident ++ "'"
    | otherwise = Map.insert ident sig m