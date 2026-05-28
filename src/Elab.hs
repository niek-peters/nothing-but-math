module Elab (elab, ElabResult) where

import qualified Data.Map as Map
import AST (Id, Signature (..), AST(..), Declaration (..), Expr (..), Type (..), PrimitiveType (..), BinaryOp (..), UnaryOp (..), Local (..), Implementation (..), Branch (..), WhereTerm (..))
import IR (IRExpr (..), IRBinaryOp (..), IR (..), IRDeclaration (IRDeclaration), IRImplementation (IRUnconditional, IRConditional), IRBranch (IRBranch), IRLocal (..), IRWhereTerm (IRLocalDecl, IRConstraint))
import Data.List.NonEmpty (NonEmpty(..), toList, fromList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Functor
import Types
import Parser (ParseResult)

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

-- So, we will not (always) be having compile-time type errors for mismatches in number types, instead inserting many type casts automatically and trusting the developer that the given type signature is correct

-- Another important thing to mention is that constraints are evaluated in order, from how they were written in the DSL, from left to right

-- TODO: make error messages always show the part of the AST where it went wrong

type ElabResult = [Fragment String IR]

type Scope = Map.Map Id Signature
type Scopes = (Scope, Scope)    -- local scope, global scope

elab :: ParseResult -> ElabResult
elab frags = map elabFragment frags
    where   globals = collectGlobals frags

            elabFragment (TextFragment str) = TextFragment str
            elabFragment (CodeFragment (AST decls)) = CodeFragment $ IR $ map (`elabDeclaration` globals) decls

collectGlobals :: ParseResult -> Scope
collectGlobals frags = foldl collect Map.empty frags
    where   collect m (TextFragment _) = m
            collect m (CodeFragment ast) = collectGlobalsFromAST m ast

collectGlobalsFromAST :: Scope -> AST -> Scope
collectGlobalsFromAST m (AST decls) = foldl insertDeclaration m decls
    where   insertDeclaration scope (Declaration ident sig _ _ _) = insertIdent scope ident sig

insertIdent :: Scope -> Id -> Signature -> Scope
insertIdent m ident sig 
    | Map.member ident m = error $ "ERROR: Duplicate declaration of '" ++ ident ++ "'"
    | otherwise = Map.insert ident sig m

-- boolean in returned tuple indicates whether the reference is to a global identifier
resolveIdent :: Id -> (Scope, Scope) -> (Signature, Bool)
resolveIdent ident (locals, globals) = case Map.lookup ident locals of
    Just sig -> (sig, False)
    Nothing -> case Map.lookup ident globals of
        Just sig -> (sig, True)
        Nothing -> error $ "ERROR: Reference to undefined value '" ++ ident ++  "'"

elabDeclaration :: Declaration -> Scope -> IRDeclaration
elabDeclaration (Declaration ident sig@(Signature from to) params impl whereTerms) globals 
    = IRDeclaration ident sig params resImpl merged
    where   resImpl = elabImplementation impl (Just to) scopes

            -- and finally we merge the locals and constraints again in their original order
            merged = mergeOrdered resLocals resConstraints
            mergeOrdered ls [] = map (IRLocalDecl . snd) ls
            mergeOrdered [] rs = map (IRConstraint . snd) rs
            mergeOrdered ls@((il, l):lss) rs@((ir, r):rss)  | il < ir = (IRLocalDecl l) : mergeOrdered lss rs
                                                            | otherwise = (IRConstraint r) : mergeOrdered ls rss

            resConstraints = map (elabCondition `applySnd`) constraintTuples
            applySnd f (a, b) = (a, f b)
            elabCondition e = fst $ elabExpr e (Just $ Type $ pure Boolean) scopes

            scopes = (localScope, globals)

            -- we then add the local declarations to the local scope
            localScope = collectLocals localDecls tmpScopes

            -- we elaborate the locals, retaining the indices in the resLocals
            (localDecls, resLocals) = unzip $ map ((`elabLocal` tmpScopes) `handleIndex`) localTuples
            handleIndex f (i, x) = (l, (i, r))
                where   (l, r) = f x
            tmpScopes = (tmpLocalScope, globals)

            -- we partition the whereTerms into locals and constraints, retaining the indices we got before
            (localTuples, constraintTuples) = foldr partitionTerms ([], []) whereTermTuples
            partitionTerms (i, LocalDecl local) (acc1, acc2) = ((i, local) : acc1, acc2)
            partitionTerms (i, Constraint cons) (acc1, acc2) = (acc1, (i, cons) : acc2)

            -- now we enumerate the whereTerms to allow use to merge them back together in the original order at the end
            whereTermTuples = zip [(0 :: Int)..] whereTerms

            -- we initialize the local scope with the function parameters
            tmpLocalScope = collectLocals (elabParams from params) (Map.empty, globals)

elabParams :: Maybe Type -> [Id] -> [(NonEmpty Id, Type)]
elabParams Nothing [] = []
elabParams Nothing params = error $ "TYPE ERROR: Constant signature specifies no parameters but implementation lists " ++ show (length params) ++ ", namely: " ++ show params
elabParams (Just (Type pts)) params
    | paramTypeCount /= paramCount = error $ "TYPE ERROR: Function signature specifies " ++ show paramTypeCount ++ " parameters but implementation lists " ++ show paramCount ++ ", namely: " ++ show params
    | otherwise = zip (map pure params) (map (Type . pure) (toList pts))
    where   paramTypeCount = length pts
            paramCount = length params

collectLocals :: [(NonEmpty Id, Type)] -> Scopes -> Scope
collectLocals decls (locals, globals) = foldl collect locals decls
    where   collect m decl = elabDestructure (m, globals) decl

-- NOTE: for now we disallow referencing locals in other locals
elabLocal :: Local -> Scopes -> ((NonEmpty Id, Type), IRLocal)
elabLocal (Local idents expr) scopes = ((idents, resType), (IRLocal idents resExpr))
    where   (resExpr, resType) = elabExpr expr Nothing scopes

elabDestructure :: Scopes -> (NonEmpty Id, Type) -> Scope
elabDestructure (locals, globals) (idents, (Type pts)) 
                | identCount /= valueCount = error $ "TYPE ERROR: Tuple destructuring has member count different from value. Value had " ++ show valueCount ++ " values, but tried to destructure to " ++ show identCount ++ " members for identifiers '" ++ show idents ++ "'"
                | otherwise = foldl insertLocal locals (NonEmpty.zip idents pts)
    where   identCount = length idents
            valueCount = length pts

            insertLocal locals' (ident, pt) = case Map.lookup ident globals of
                Just _ -> error $ "ERROR: Local declaration of '" ++ ident ++ "' is disallowed as it shadows a global declaration of the same name"
                Nothing -> insertIdent locals' ident (Signature Nothing (Type $ pure pt))

elabImplementation :: Implementation -> Maybe Type -> Scopes -> IRImplementation
elabImplementation (Unconditional expr) mt scopes = IRUnconditional $ fst $ elabExpr expr mt scopes
elabImplementation (Conditional ifs other) mt scopes = IRConditional resIfs resOther
    where   resIfs = map (\branch -> elabBranch branch mt scopes) ifs
            resOther = fst $ elabExpr other mt scopes

elabBranch :: Branch -> Maybe Type -> Scopes -> IRBranch
elabBranch (Branch e1 e2) mt scopes = IRBranch resE1 resE2
    where   resE1 = fst $ elabExpr e1 mt scopes
            resE2 = fst $ elabExpr e2 (Just $ Type $ pure Boolean) scopes

-- expression to elaborate, possible requested type of expression, identifier scopes, elaborated expression with type 
elabExpr :: Expr -> Maybe Type -> Scopes -> (IRExpr, Type)
elabExpr (Call ident []) mt scopes = case maybeFrom of
    Just from -> error $ "TYPE ERROR: Call to function '" ++ ident ++ "' missing args '" ++ show from ++ "'"
    Nothing -> maybeCastExpr (IRCall ident isGlobal []) to mt
    where   (Signature maybeFrom to, isGlobal) = resolveIdent ident scopes
elabExpr (Call ident es) mt scopes = case maybeFrom of
    Nothing -> error $ "TYPE ERROR: Reference to constant '" ++ ident ++ "' incorrectly called like a function with args: " ++ show es
    Just _ -> maybeCastExpr (IRCall ident isGlobal (toList resArgs)) to mt
    where   (resArgs, _) = elabExprs (fromList es) maybeFrom scopes
            (Signature maybeFrom to, isGlobal) = resolveIdent ident scopes
elabExpr (ImmediateInt i) mt _ = maybeCastExpr (IRImmediateInt i pt) (Type $ pure pt) mt
    where   pt  | i < 0 = Integer
                | i > 0 = Positive
                | otherwise = Natural
elabExpr (ImmediateReal r) mt _ = maybeCastExpr (IRImmediateReal r) (Type $ pure Real) mt
elabExpr (ImmediateBool b) mt _ = maybeCastExpr (IRImmediateBool b) (Type $ pure Boolean) mt
elabExpr (Binary op e1 e2) mt scopes = maybeCastExpr resExpr resType mt
    where   (resExpr, resType) = elabBinary op resE1 resE2
            resE1 = elabExpr e1 Nothing scopes  -- NOTE: right now we don't infer requested operand types from the requested type
            resE2 = elabExpr e2 Nothing scopes
elabExpr (Unary op e) mt scopes = maybeCastExpr resExpr resType mt
    where   (resExpr, resType) = elabUnary op resE
            resE = elabExpr e Nothing scopes  -- NOTE: right now we don't infer the requested operand type from the requested type
elabExpr (Tuple es) mt scopes = (IRTuple resExprs, resType)
    where   (resExprs, resType) = elabExprs es mt scopes

-- used for tuples and function calls
elabExprs :: NonEmpty Expr -> Maybe Type -> Scopes -> (NonEmpty IRExpr, Type)
elabExprs es mt scopes = (resExprs, Type resType)
    where   (resExprs, resType) = Data.Functor.unzip irExprs
            irExprs = NonEmpty.map (\(expr, mt1) -> convert $ elabExpr expr mt1 scopes) (zipMaybeTypes es mt)  -- we pass the requested type from the requested tuple type
            
            -- ensures tuple items or function call arguments are not tuples (nesting is not allowed)
            convert (e1, (Type (t :| []))) = (e1, t)
            convert (e1, _) = error $ "TYPE ERROR: Cannot use tuples inside other tuples or as function call arguments. Encountered nested tuple: " ++ show e1

zipMaybeTypes :: NonEmpty a -> Maybe Type -> NonEmpty (a, Maybe Type)
zipMaybeTypes a Nothing = NonEmpty.map (\c -> (c, Nothing)) a
zipMaybeTypes a (Just (Type b))
    | actualLength /= expectedLength = error $ "TYPE ERROR: Expected tuple or function call with " ++ show expectedLength ++ " elements, but got " ++ show actualLength ++ " elements"
    | otherwise = NonEmpty.map (\(c, d) -> (c, Just $ Type $ pure d)) (NonEmpty.zip a b)
    where   actualLength = length a
            expectedLength = length b

elabUnary :: UnaryOp -> (IRExpr, Type) -> (IRExpr, Type)
elabUnary Sqrt (e, (Type (t :| [])))
    | t /= Boolean = (IRUnary Sqrt e, Type $ pure Real) -- we use a pattern guard to fall through to the other cases if false

-- floor operation is not defined for integer types
elabUnary Floor (e, (Type (Real :| []))) = (IRUnary Floor e, Type $ pure Integer)
elabUnary Floor (e, (Type (Rational :| []))) = (IRUnary Floor e, Type $ pure Integer)

-- any remaining undefined operations
elabUnary op (_, t) = error $ "TYPE ERROR: Unary operation '" ++ show op ++ "' not defined for type '" ++ show t ++ "'"


elabBinary :: BinaryOp -> (IRExpr, Type) -> (IRExpr, Type) -> (IRExpr, Type) 
-- we have a special case for integer powers, which don't do any type casting and have the result keep the type of the left operand
elabBinary Pow (e1, t1@(Type (pt1 :| []))) (e2, (Type (pt2 :| [])))
    | pt1 /= Boolean && (pt2 == Positive || pt2 == Natural || pt2 == Integer) = (IRBinary IRPow e1 e2, t1)  -- we use a pattern guard to fall through to the generic case if false

-- and then the generic case where both operands get casted to the same type
elabBinary op o1@(_, (Type (pt1 :| []))) o2@(_, (Type (pt2 :| []))) = (resExpr, resType)
    where   resExpr = sameOperandTypesBinary op o1 o2 operandType
            resType = Type $ pure $ binaryResType op operandType
            operandType | op == Sub || op == Div        = max greaterType Integer   -- prevents underflow and forces fractions to be Ratio Integer
                        | op == Mod || op == Divides    = min greaterType Integer   -- these operations are only legal for whole numbers
                        | otherwise = greaterType
            greaterType = max pt1 pt2      -- crashes on Boolean

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

sameOperandTypesBinary :: BinaryOp -> (IRExpr, Type) -> (IRExpr, Type) -> PrimitiveType -> IRExpr
sameOperandTypesBinary op (e1, t1) (e2, t2) pt = IRBinary (toIRBinaryOp op t1 t2) operand1 operand2
    where   operand1 = fst $ maybeCastExpr e1 t1 justResType
            operand2 = fst $ maybeCastExpr e2 t2 justResType
            justResType = Just resType
            resType = Type $ pure pt

-- assume the special integer power case is already handled
toIRBinaryOp :: BinaryOp -> Type -> Type -> IRBinaryOp
toIRBinaryOp Add _ _ = IRAdd
toIRBinaryOp Sub _ _ = IRSub
toIRBinaryOp Mult _ _ = IRMult
toIRBinaryOp Div t1 t2
    | isIntegerType t1 && isIntegerType t2 = IRFrac -- construct a fraction whenever neither operand of a division is at least a fraction yet
toIRBinaryOp Div _ _ = IRDiv
toIRBinaryOp Pow _ _ = IRExp
toIRBinaryOp Mod _ _ = IRMod
toIRBinaryOp Eq _ _ = IREq
toIRBinaryOp Neq _ _ = IRNeq
toIRBinaryOp Less _ _ = IRLess
toIRBinaryOp Greater _ _ = IRGreater
toIRBinaryOp LessEq _ _ = IRLessEq
toIRBinaryOp GreaterEq _ _ = IRGreaterEq
toIRBinaryOp Divides _ _ = IRDivides

isIntegerType :: Type -> Bool
isIntegerType (Type (Positive :| [])) = True
isIntegerType (Type (Natural :| [])) = True
isIntegerType (Type (Integer :| [])) = True
isIntegerType _ = False

maybeCastExpr :: IRExpr -> Type -> Maybe Type -> (IRExpr, Type)
maybeCastExpr e f Nothing = (e, f)
maybeCastExpr e f (Just t)  | f == t = (e, f)
                            | otherwise = castExpr e f t

castExpr :: IRExpr -> Type -> Type -> (IRExpr, Type)
castExpr (IRTuple es) (Type f) (Type t) 
    | length f == length t = (IRTuple resExprs, Type resType)
    where   (resExprs, resType) = Data.Functor.unzip castExprs
            castExprs = NonEmpty.map (\(e, (pf, pt)) -> castPrimitiveExpr e pf pt) (NonEmpty.zip es (NonEmpty.zip f t))
castExpr e (Type (f :| [])) (Type (t :| [])) = (resExpr, Type $ pure resType)
    where   (resExpr, resType) = castPrimitiveExpr e f t
castExpr e f t = error $ "TYPE ERROR: Cannot cast from '" ++ show f ++ "' to '" ++ show t ++ "'. Tried casting expression: " ++ show e

castPrimitiveExpr :: IRExpr -> PrimitiveType -> PrimitiveType -> (IRExpr, PrimitiveType)
castPrimitiveExpr e f t | isPrimitiveCastLegal f t = (IRCast e f t, t)
                        | otherwise = error $ "TYPE ERROR: Cannot cast from '" ++ show f ++ "' to '" ++ show t ++ "'. Tried casting expression: " ++ show e

-- assumes types are not equal
isPrimitiveCastLegal :: PrimitiveType -> PrimitiveType -> Bool
isPrimitiveCastLegal Boolean _      = False -- casting to/from Booleans is illegal
isPrimitiveCastLegal _ Boolean      = False
isPrimitiveCastLegal _ _            = True  -- all casting between number types is legal
