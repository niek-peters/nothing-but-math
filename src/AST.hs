module AST (module AST) where -- export everything

import Data.List.NonEmpty (NonEmpty)
import Token (PrimitiveType, UnaryOp, BinaryOp)

type Id = String

data AST = AST [BlockAnnotation] [Declaration]
    deriving (Show, Eq)

data Declaration = Declaration 
    [DeclAnnotation]
    Id              -- declaration name
    Signature       -- type signature
    Id              -- implementation name (checked by elab if it matches declaration name)
    [Id]            -- arguments
    Implementation  
    [WhereTerm]
    deriving (Show, Eq)

data Signature = Signature 
    (Maybe Type)    -- argument type
    Type            -- return type
    deriving (Show, Eq)

newtype Type = Type (NonEmpty PrimitiveType)
    deriving (Show, Eq)

data Implementation = Unconditional Expr 
                    | Conditional [Branch] Expr -- piecewise function. Expr is otherwise branch
    deriving (Show, Eq)

data Branch = Branch Expr Expr  -- Expr if Expr
    deriving (Show, Eq)

data WhereTerm = LocalDecl Local | Constraint Expr
    deriving (Show, Eq)

data Local = Local 
    (NonEmpty Id)   -- singular usually, but also allows tuple destructuring, e.g. (a, b) := f(x)
    Expr  
    deriving (Show, Eq)

data Expr   = Call Id [Expr]
            | ImmediateInt Integer
            | ImmediateReal Double
            | ImmediateBool Bool
            | Binary BinaryOp Expr Expr     
            | Unary UnaryOp Expr         -- E.g. sqrt(a)
            | Tuple (NonEmpty Expr)      -- E.g. (a, b, c) 
    deriving (Show, Eq)

-- block annotation for LaTeX output
-- could support multiple different annotations in the future
-- the elaboration phase ensures there are no duplicate annotations
data BlockAnnotation = BlockDisplay BlockDisplayMode | BlockClass String | BlockName String | BlockLabel String | BlockDescription String
    deriving (Show, Eq)

data BlockDisplayMode   = DefaultBlock  -- outputs block in \begin{flalign*}...\end{flalign*} block (this can not be parsed, only set by the elaboration phase)
                        | BoxBlock      -- outputs nicely boxed block
                        | InTextBlock   -- outputs block as in-text lines wrapped with $
                        | InLineBlock   -- outputs block as a single line wrapped with $
                        | HiddenBlock   -- omits the block from LaTeX output
    deriving (Show, Eq)

-- declaration annotation for LaTeX output
-- could support multiple different annotations in the future
-- the elaboration phase ensures there are no duplicate annotations
data DeclAnnotation = DeclDisplay DeclDisplayMode
    deriving (Show, Eq)

data DeclDisplayMode    = DefaultDecl   -- declaration is emitted as usual (this can not be parsed, only set by the elaboration phase)
                        | HiddenDecl    -- omits the declaration from LaTeX output
    deriving (Show, Eq)