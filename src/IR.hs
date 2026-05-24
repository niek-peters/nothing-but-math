module IR where

import AST (Id, Signature, BinaryOp, UnaryOp, Type, PrimitiveType)
import Data.List.NonEmpty (NonEmpty)

newtype IR = IR [IRDeclaration]
    deriving (Show, Eq)

data IRDeclaration = IRDeclaration 
    Id              -- name
    Signature       -- type signature
    [Id]            -- arguments
    IRImplementation  
    [IRLocal]         -- local declarations
    [IRExpr]          -- constraints
    deriving (Show, Eq)


data IRImplementation   = IRUnconditional IRExpr 
                        | IRConditional [IRBranch] IRExpr -- piecewise function. Expr is otherwise branch
    deriving (Show, Eq)

data IRBranch = IRBranch IRExpr IRExpr  -- Expr if Expr
    deriving (Show, Eq)

data IRLocal = IRLocal 
    (NonEmpty Id)   -- singular usually, but also allows tuple destructuring, e.g. (a, b) := f(x)
    IRImplementation  
    deriving (Show, Eq)

data IRExpr = IRCast IRExpr PrimitiveType PrimitiveType       -- wraps an expr that should be cast from type to type
            | IRCall Id [IRExpr]
            | IRImmediateInt Int
            | IRImmediateReal Double
            | IRImmediateBool Bool
            | IRBinary IRBinaryOp IRExpr IRExpr     
            | IRUnary UnaryOp IRExpr         -- E.g. sqrt(a)
            | IRTuple (NonEmpty IRExpr)      -- E.g. (a, b, c) 
    deriving (Show, Eq)

-- here we differentiate between Pow (Haskell ^, integer power) and Exp (Haskell **, rational/real power)
data IRBinaryOp = IRAdd | IRSub | IRMult | IRDiv | IRPow | IRExp | IRMod | IREq | IRNeq | IRLess | IRGreater | IRLessEq | IRGreaterEq | IRDivides
    deriving (Show, Eq)