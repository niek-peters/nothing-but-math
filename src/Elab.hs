module Elab where
import Parser (ParseResult(..), Fragment (..))
import qualified Data.Map as Map
import AST (Id, Signature (..), AST(..), Declaration (..), Expr (..), Type (..), PrimitiveType (..), BinaryOp (..), UnaryOp (..))
import IR (IRExpr (..), IRBinaryOp (..))
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

type Scope = Map.Map Id Signature
-- type LocalScope = Map.Map Id Type
type Scopes = (Scope, Scope)    -- local scope, global scope

collectGlobals :: ParseResult -> Scope
collectGlobals (ParseResult frags) = foldl collect Map.empty frags
    where   collect m (TextFragment _) = m
            collect m (CodeFragment ast) = collectGlobalsFromAST m ast

collectGlobalsFromAST :: Scope -> AST -> Scope
collectGlobalsFromAST m (AST decls) = foldl insertDeclaration m decls

insertDeclaration :: Scope -> Declaration -> Scope
insertDeclaration m (Declaration ident sig _ _ _ _) 
    | Map.member ident m = error $ "ERROR: Duplicate declaration of '" ++ show ident ++ "'"
    | otherwise = Map.insert ident sig m

resolveIdent :: Id -> (Scope, Scope) -> Signature
resolveIdent ident (locals, globals) = case Map.lookup ident locals of
    Just sig -> sig
    Nothing -> case Map.lookup ident globals of
        Just sig -> sig
        Nothing -> error $ "ERROR: Reference to undefined value '" ++ show ident ++  "'"

-- expression to elaborate, possible requested type of expression, identifier scopes, elaborated expression with type 
elabExpr :: Expr -> Maybe Type -> Scopes -> (IRExpr, Type)
-- elabExpr e@(Call ident []) mt scopes    | from == Nothing = maybeCastExpr (IRCall ident []) to mt
--     where   (Signature from to) = resolveIdent ident scopes
-- elabExpr e@(Call ident es) mt scopes   = error "GG" 
--     where   sig = resolveIdent ident scopes

--             irExprs = map (\expr -> convert $ elabExpr expr Nothing scopes) es  -- right now we don't pass the requested type from the requested function argument types

--             -- ensures arguments are not tuples
--             convert (e1, (Type (t :| []))) = (e1, t)
--             convert _ = error $ "TYPE ERROR: Cannot call function with tuple. Tried passing tuple in call: " ++ show e
elabExpr (ImmediateInt i) mt _ = maybeCastExpr (IRImmediateInt i) pt mt
    where   pt  | i < 0 = Integer
                | i > 0 = Positive
                | otherwise = Natural
elabExpr (ImmediateReal r) mt _ = maybeCastExpr (IRImmediateReal r) (Type $ pure Real) mt
elabExpr (ImmediateBool b) mt _ = maybeCastExpr (IRImmediateBool b) (Type $ pure Boolean) mt
elabExpr (Binary op e1 e2) mt scopes = maybeCastExpr resExpr resType mt
    where   (resExpr, resType) = elabBinary op resE1 resE2
            resE1 = elabExpr e1 Nothing scopes  -- right now we don't infer requested operand types from the requested type
            resE2 = elabExpr e2 Nothing scopes
elabExpr (Unary op e) mt scopes = maybeCastExpr resExpr resType mt
    where   (resExpr, resType) = elabUnary op resE
            resE = elabExpr e Nothing scopes  -- right now we don't infer the requested operand type from the requested type
elabExpr e@(Tuple es) mt scopes = maybeCastExpr (IRTuple resExprs) (Type resType) mt 
    where   (resExprs, resType) = Data.Functor.unzip irExprs
            irExprs = NonEmpty.map (\expr -> convert $ elabExpr expr Nothing scopes) es  -- right now we don't pass the requested type from the requested tuple type
            
            -- ensures tuple items are not tuples themselves
            convert (e1, (Type (t :| []))) = (e1, t)
            convert _ = error $ "TYPE ERROR: Cannot nest tuples. Tried nesting in expression: " ++ show e

elabExpr _ _ _ = error "GG"


elabUnary :: UnaryOp -> (IRExpr, Type) -> (IRExpr, Type)
elabUnary Sqrt (e, (Type (t :| [])))
    | t /= Boolean = (IRUnary Sqrt e, Type $ pure Real) -- we use a pattern guard to fall through to the other cases if false

-- floor operation is not defined for integer types
elabUnary Floor (e, (Type (Real :| []))) = (IRUnary Floor e, Type $ pure Integer)
elabUnary Floor (e, (Type (Rational :| []))) = (IRUnary Floor e, Type $ pure Integer)

-- any remaining undefined operations
elabUnary op (_, t) = error $ "TYPE ERROR: Unary operation '" ++ show op ++ "' not defined for type '" ++ show t ++ "'"


elabBinary :: BinaryOp -> (IRExpr, PrimitiveType) -> (IRExpr, PrimitiveType) -> (IRExpr, PrimitiveType) 
-- we have a special case for integer powers, which don't do any type casting and have the result keep the type of the left operand
elabBinary Pow (e1, t1) (e2, t2)
    | t1 /= Boolean && (t2 == Positive || t2 == Natural || t2 == Integer) = (IRBinary IRPow e1 e2, t1)  -- we use a pattern guard to fall through to the generic case if false

-- and then the generic case where both operands get casted to the same type
elabBinary op o1@(_, t1) o2@(_, t2) = (resExpr, resType)
    where   resExpr = sameOperandTypesBinary op o1 o2 operandType
            resType = binaryResType op operandType
            operandType | op == Sub || op == Div        = toAtLeastInteger greaterType  -- prevents underflow and forces fractions to be Ratio Integer
                        | op == Mod || op == Divides    = toAtMostInteger greaterType
                        | otherwise = greaterType
            greaterType = getGreaterNumberType t1 t2      -- crashes on Boolean

-- any remaining undefined operations
elabBinary op (_, t1) (_, t2) = error $ "TYPE ERROR: Binary operation '" ++ show op ++ "' not defined for types '" ++ show t1 ++ "' and '" ++ show t2 ++ "'"

binaryResType :: BinaryOp -> PrimitiveType -> PrimitiveType
binaryResType Div Real = Real
binaryResType Div _ = Rational
binaryResType Eq _ = Boolean
binaryResType Neq _ = Boolean
binaryResType Less _ = Boolean
binaryResType Greater _ = Boolean
binaryResType LessEq _ = Boolean
binaryResType GreaterEq _ = Boolean
binaryResType Divides _ = Boolean
binaryResType _ t = t  -- generic case where resulting type is the same as the operands

toAtLeastInteger :: PrimitiveType -> PrimitiveType
toAtLeastInteger Positive = Integer
toAtLeastInteger Natural = Integer
toAtLeastInteger t = t

toAtLeastRational :: PrimitiveType -> PrimitiveType
toAtLeastRational Positive = Rational
toAtLeastRational Natural = Rational
toAtLeastRational Integer = Rational
toAtLeastRational t = t

toAtMostInteger :: PrimitiveType -> PrimitiveType
toAtMostInteger Real = Integer      -- these two will throw a type error in castExpr
toAtMostInteger Rational = Integer  -- I opted to have it throw there so we get the full nice error message
toAtMostInteger t = t

sameOperandTypesBinary :: BinaryOp -> (IRExpr, PrimitiveType) -> (IRExpr, PrimitiveType) -> PrimitiveType -> IRExpr
sameOperandTypesBinary op (e1, t1) (e2, t2) pt = IRBinary (toIRBinaryOp op) operand1 operand2
    where   operand1 = fst $ maybeCastExpr e1 t1 justResType
            operand2 = fst $ maybeCastExpr e2 t2 justResType
            justResType = Just pt

-- assume the special integer power case is already handled
toIRBinaryOp :: BinaryOp -> IRBinaryOp
toIRBinaryOp Add = IRAdd
toIRBinaryOp Sub = IRSub
toIRBinaryOp Mult = IRMult
toIRBinaryOp Div = IRDiv
toIRBinaryOp Pow = IRExp
toIRBinaryOp Mod = IRMod
toIRBinaryOp Eq = IREq
toIRBinaryOp Neq = IRNeq
toIRBinaryOp Less = IRLess
toIRBinaryOp Greater = IRGreater
toIRBinaryOp LessEq = IRLessEq
toIRBinaryOp GreaterEq = IRGreaterEq
toIRBinaryOp Divides = IRDivides

-- assumes types are number types
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

-- maybeCastExprs :: NonEmpty IRExpr -> Type -> Maybe Type -> (NonEmpty IRExpr, Type)
-- maybeCastExprs es 

maybeCastExpr :: IRExpr -> PrimitiveType -> Maybe PrimitiveType -> (IRExpr, PrimitiveType)
maybeCastExpr e f Nothing = (e, f)
maybeCastExpr e f (Just t)  | f == t = (e, f)
                            | otherwise = castExpr e f t

castExpr :: IRExpr -> PrimitiveType -> PrimitiveType -> (IRExpr, PrimitiveType)
castExpr e f t  | isTypeCastLegal f t = (IRCast e f t, t)
                | otherwise = error $ "TYPE ERROR: Cannot cast from '" ++ show f ++ "' to '" ++ show t ++ "'. Tried casting expression: " ++ show e

isTypeCastLegal :: PrimitiveType -> PrimitiveType -> Bool
isTypeCastLegal f t | f == t = True
                    | otherwise = isPrimitiveCastLegal f t

-- assumes types are not equal
isPrimitiveCastLegal :: PrimitiveType -> PrimitiveType -> Bool
isPrimitiveCastLegal Boolean _      = False 
isPrimitiveCastLegal _ Boolean      = False
isPrimitiveCastLegal Real Rational  = True      -- down-casting from Reals/Rationals to Integers requires an explicit floor
isPrimitiveCastLegal Real _         = False
isPrimitiveCastLegal Rational Real  = True
isPrimitiveCastLegal Rational _     = False
isPrimitiveCastLegal _ _            = True

-- maybeCastExpr :: IRExpr -> Type -> Maybe Type -> (IRExpr, Type)
-- maybeCastExpr e f Nothing = (e, f)
-- maybeCastExpr e f (Just t)  | f == t = (e, f)
--                             | otherwise = castExpr e f t

-- castExpr :: IRExpr -> Type -> Type -> (IRExpr, Type)
-- castExpr e f t  | isTypeCastLegal f t = (IRCast e f t, t)
--                 | otherwise = error $ "TYPE ERROR: Cannot cast from '" ++ show f ++ "' to '" ++ show t ++ "'. Tried casting expression: " ++ show e

-- isTypeCastLegal :: Type -> Type -> Bool
-- isTypeCastLegal (Type f) (Type t)   | length f /= length t = False
--                                     | otherwise = all (\(a, b) -> a == b || isPrimitiveCastLegal a b) (NonEmpty.zip f t)

-- -- assumes types are not equal
-- isPrimitiveCastLegal :: PrimitiveType -> PrimitiveType -> Bool
-- isPrimitiveCastLegal Boolean _      = False 
-- isPrimitiveCastLegal _ Boolean      = False
-- isPrimitiveCastLegal Real Rational  = True      -- down-casting from Reals/Rationals to Integers requires an explicit floor
-- isPrimitiveCastLegal Real _         = False
-- isPrimitiveCastLegal Rational Real  = True
-- isPrimitiveCastLegal Rational _     = False
-- isPrimitiveCastLegal _ _            = True
