module AST where

import Data.List.NonEmpty (NonEmpty)

type Id = String

newtype AST = AST [Declaration]
    deriving (Show, Eq)

data Declaration = Declaration 
    Id              -- name
    Signature       -- type signature
    [Id]            -- arguments
    Implementation  
    [Local]         -- local declarations
    [Expr]          -- constraints
    deriving (Show, Eq)

data Signature = Signature 
    (Maybe Type)    -- argument type
    Type            -- return type
    deriving (Show, Eq)

newtype Type = Type (NonEmpty PrimitiveType)
    deriving (Show, Eq)

data PrimitiveType = Positive | Natural | Integer | Rational | Real
    deriving (Show, Eq)

data Implementation = Unconditional Expr 
                    | Conditional [Branch] Expr -- piecewise function. Expr is otherwise branch
    deriving (Show, Eq)

data Branch = Branch Expr Expr  -- Expr if Expr
    deriving (Show, Eq)

data Local = Local 
    (NonEmpty Id)   -- singular usually, but also allows tuple destructuring, e.g. (a, b) := f(x)
    Implementation  
    deriving (Show, Eq)

data Expr   = Call Id [Expr]
            | ImmediateInt Int
            | ImmediateReal Double
            | ImmediateBool Bool
            | Binary BinaryOp Expr Expr     
            | Unary UnaryOp Expr         -- E.g. sqrt(a)
            | Tuple [Expr]               -- E.g. (a, b, c) 
    deriving (Show, Eq)

data BinaryOp = Add | Sub | Mult | Div | Pow | Mod | Eq | Neq | Less | Greater | LessEq | GreaterEq | Divides
    deriving (Show, Eq)

data UnaryOp = Sqrt | Floor
    deriving (Show, Eq)
