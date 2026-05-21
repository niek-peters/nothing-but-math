module Elab where
import Parser (ParseResult(..), Fragment (..))
import qualified Data.Map as Map
import AST (Id, Signature, AST(..), Declaration (..), Expr (..), Type (..), PrimitiveType (..), BinaryOp (..))
import IR (IRExpr (..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Functor

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
type LocalScope = Map.Map Id Type
type Scopes = (LocalScope, GlobalScope)

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


-- expression to elaborate, possible requested type of expression, identifier scopes, elaborated expression with type 
elabExpr :: Expr -> Maybe Type -> Scopes -> (IRExpr, Type)
elabExpr (ImmediateInt i) mt _ = maybeCastExpr (IRImmediateInt i) (Type $ pure pt) mt
    where   pt  | i < 0 = Integer
                | i > 0 = Positive
                | otherwise = Natural
elabExpr (ImmediateReal r) mt _ = maybeCastExpr (IRImmediateReal r) (Type $ pure Real) mt
elabExpr (ImmediateBool b) mt _ = maybeCastExpr (IRImmediateBool b) (Type $ pure Boolean) mt
elabExpr e@(Tuple es) mt scopes = maybeCastExpr (IRTuple resExprs) (Type resType) mt 
    where   (resExprs, resType) = Data.Functor.unzip irExprs
            irExprs = NonEmpty.map (\expr -> convert $ elabExpr expr Nothing scopes) es  -- right now we don't pass the requested type from the requested tuple type
            
            -- ensures tuple items are not tuples themselves
            convert (e1, (Type (t :| []))) = (e1, t)
            convert _ = error $ "TYPE ERROR: Cannot nest tuples. Tried nesting in expression: " ++ show e
elabExpr (Binary op e1 e2) mt scopes = maybeCastExpr resExpr resType mt
    where   (resExpr, resType) = elabBinary op resE1 resE2
            resE1 = elabExpr e1 Nothing scopes  -- right now we don't infer requested operand types from the requested type
            resE2 = elabExpr e2 Nothing scopes

-- elabExpr (ImmediateInt i) mt _ = case mt of
--                     Nothing -> (IRImmediateInt i, t1)
--                     Just rt | rt == t1 -> (IRImmediateInt i, t1)
--                             | not $ isNumberType rt -> error $ "TYPE ERROR: Cannot cast from '" ++ show t1 ++ "' to '" ++ show rt ++ "'"
--                             | otherwise -> (IRCast (IRImmediateInt i) t1 rt, rt)
--     where   t1 = Type $ pure pt
--             pt  | i < 0 = Integer
--                 | i > 0 = Positive
--                 | otherwise = Natural
-- elabExpr (ImmediateReal r) mt _ = case mt of
--                     Nothing -> (IRImmediateReal r, t1)
--                     Just rt | rt == t1 -> (IRImmediateReal r, t1)
--                             | rt == (Type $ (Rational :| [])) -> (IRCast (IRImmediateReal r) t1 rt, rt)
--                             | otherwise -> error $ "TYPE ERROR: Cannot cast from '" ++ show t1 ++ "' to '" ++ show rt ++ "'"
--     where   t1 = Type $ pure Real
-- elabExpr (ImmediateBool b) mt _ = case mt of
--                     Nothing -> (IRImmediateBool b, t1)
--                     Just rt | rt == t1 -> (IRImmediateBool b, t1)
--                             | otherwise -> error $ "TYPE ERROR: Cannot cast from '" ++ show t1 ++ "' to '" ++ show rt ++ "'"
--     where   t1 = Type $ pure Boolean  
-- elabExpr (ImmediateInt i) mt _ = (IRImmediateInt i, t2)
--     where   t2 = case mt of
--                     Nothing -> t1
--                     Just rt | not $ isNumberType rt -> error $ "TYPE ERROR: Cannot cast from '" ++ show t1 ++ "' to '" ++ show rt ++ "'"
--                             | otherwise -> rt
--             t1 = Type $ pure pt
--             pt  | i < 0 = Integer
--                 | i > 0 = Positive
--                 | otherwise = Natural
elabExpr _ _ _ = error "GG"

elabBinary :: BinaryOp -> (IRExpr, Type) -> (IRExpr, Type) -> (IRExpr, Type) 
elabBinary Add (e1, t1@(Type (pt1 :| []))) (e2, t2@(Type (pt2 :| []))) = (IRBinary Add operand1 operand2, resType)
    where   operand1 = fst $ maybeCastExpr e1 t1 justResType
            operand2 = fst $ maybeCastExpr e2 t2 justResType
            justResType = Just resType
            resType | t1 == t2 = t1
                    | otherwise = Type $ pure $ getGreaterNumberType pt1 pt2

-- assumes types are not equal
-- also assumes types are number types
getGreaterNumberType :: PrimitiveType -> PrimitiveType -> PrimitiveType
getGreaterNumberType Boolean _ = error "LOGIC ERROR: getGreaterNumberType called with Boolean type"
getGreaterNumberType _ Boolean = error "LOGIC ERROR: getGreaterNumberType called with Boolean type"
getGreaterNumberType Real _ = Real 
getGreaterNumberType _ Real = Real 
getGreaterNumberType Rational _ = Rational
getGreaterNumberType _ Rational = Rational
getGreaterNumberType Integer _ = Integer
getGreaterNumberType _ Integer = Integer
getGreaterNumberType Natural _ = Natural
getGreaterNumberType _ Natural = Natural
getGreaterNumberType Positive _ = Positive
getGreaterNumberType _ Positive = Positive

isTupleType :: Type -> Bool
isTupleType (Type (_ :| [])) = False
isTupleType _ = True


isNumberType :: Type -> Bool
isNumberType (Type (_ :| (_:_))) = False    -- tuple
isNumberType (Type (Boolean :| [])) = False
isNumberType _  = True

maybeCastExpr :: IRExpr -> Type -> Maybe Type -> (IRExpr, Type)
maybeCastExpr e f Nothing = (e, f)
maybeCastExpr e f (Just t)  | f == t = (e, f)
                            | otherwise = castExpr e f t

castExpr :: IRExpr -> Type -> Type -> (IRExpr, Type)
castExpr e f t  | isTypeCastLegal f t = (IRCast e f t, t)
                | otherwise = error $ "TYPE ERROR: Cannot cast from '" ++ show f ++ "' to '" ++ show t ++ "'. Tried casting expression: " ++ show e

isTypeCastLegal :: Type -> Type -> Bool
isTypeCastLegal (Type f) (Type t)   | length f /= length t = False
                                    | otherwise = all (\(a, b) -> a == b || isPrimitiveCastLegal a b) (NonEmpty.zip f t)

-- assumes types are not equal
isPrimitiveCastLegal :: PrimitiveType -> PrimitiveType -> Bool
isPrimitiveCastLegal Boolean _      = False 
isPrimitiveCastLegal _ Boolean      = False
isPrimitiveCastLegal Real Rational  = True
isPrimitiveCastLegal Real _         = False
isPrimitiveCastLegal _ _            = True