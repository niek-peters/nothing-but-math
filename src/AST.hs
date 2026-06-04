module AST (module AST) where -- export everything

import Data.List.NonEmpty (NonEmpty)

type Id = String

data AST = AST [BlockAnnotation] [Declaration]
    deriving (Show, Eq)

data Declaration = Declaration 
    [DeclAnnotation]
    Id              -- name
    Signature       -- type signature
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

data PrimitiveType = Positive | Natural | Integer | Rational | Real | Boolean
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

-- TODO: consider adding a parens expr for use in LaTeX codeGen, to decide whether to parenthesize an expression or not. We could also choose to make a generic thing for both targets that adds parens only when necessary
data Expr   = Call Id [Expr]
            | ImmediateInt Int
            | ImmediateReal Double
            | ImmediateBool Bool
            | Binary BinaryOp Expr Expr     
            | Unary UnaryOp Expr         -- E.g. sqrt(a)
            | Tuple (NonEmpty Expr)      -- E.g. (a, b, c) 
    deriving (Show, Eq)

data BinaryOp = Add | Sub | Mult | Div | Pow | Mod | Eq | Neq | Less | Greater | LessEq | GreaterEq | Divides
    deriving (Show, Eq)

data UnaryOp = Sqrt | Floor
    deriving (Show, Eq)

-- block annotation for LaTeX output
-- could support multiple different annotations in the future
-- the elaboration phase ensures there are no duplicate annotations
data BlockAnnotation = BlockDisplay BlockDisplayMode | BlockClass String | BlockName String | BlockLabel String
    deriving (Show, Eq)

data BlockDisplayMode   = DefaultBlock  -- outputs block in \begin{flalign*}...\end{flalign*} block
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

data DeclDisplayMode    = DefaultDecl   -- declaration is emitted as usual
                        | HiddenDecl    -- omits the declaration from LaTeX output
    deriving (Show, Eq)



-- we implement Ord for PrimitiveType to easily be able to see whether a number type is a subtype of another number type
-- this also gives us access to the min and max functions
instance Ord PrimitiveType where
    -- we order types by a simple ranking
    pt1 <= pt2 = rank pt1 <= rank pt2
      where
        rank :: PrimitiveType -> Int
        rank Positive = 1
        rank Natural  = 2
        rank Integer  = 3
        rank Rational = 4
        rank Real     = 5
        rank Boolean  = error $ "LOGIC ERROR: Attempt at comparing Boolean to other type"