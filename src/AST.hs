-- | Defines Abstract Syntax Tree which is the result of parsing.
module AST (module AST) where -- export everything

import Data.List.NonEmpty (NonEmpty)
import Token (PrimitiveType, UnaryOp, BinaryOp)

-- | Identifier type for references.
type Id = String

-- | Top-level AST containing block annotations and declarations.
data AST = AST [BlockAnnotation] [Declaration]
    deriving (Show, Eq)

-- | Top-level function/constant declaration: optional annotations, name, signature, argument identifiers, implementation and where-clause terms.
data Declaration = Declaration 
    [DeclAnnotation]
    Id              -- declaration name
    Signature       -- type signature
    Id              -- implementation name (checked by elab if it matches declaration name)
    [Id]            -- arguments
    Implementation  
    [WhereTerm]
    deriving (Show, Eq)

-- | Function signature: optional argument type and return type.
data Signature = Signature 
    (Maybe Type)    -- argument type
    Type            -- return type
    deriving (Show, Eq)

-- | (Possibly tuple) type represented as a non-empty list of primitive types.
newtype Type = Type (NonEmpty PrimitiveType)
    deriving (Show, Eq)

-- | Implementation of a function/constant: either unconditional expression or conditional branches (piecewise function/constant).
data Implementation = Unconditional Expr 
                    | Conditional [Branch] Expr -- piecewise function. Expr is otherwise branch
    deriving (Show, Eq)

-- | Conditional branch: an expression guarded by a boolean expression.
data Branch = Branch Expr Expr  -- Expr if Expr
    deriving (Show, Eq)

-- | Where-term attached to a declaration: either a local declaration or a constraint.
data WhereTerm = LocalDecl Local | Constraint Expr
    deriving (Show, Eq)

-- | Local declaration: one or more identifiers bound to an expression.
data Local = Local 
    (NonEmpty Id)   -- singular usually, but also allows tuple destructuring, e.g. (a, b) := f(x)
    Expr  
    deriving (Show, Eq)

-- | Expressions in the language: function/constant calls/references, immediate values, binary/unary ops and tuples.
data Expr   = Call Id [Expr]
        | ImmediateInt Integer
        | ImmediateReal Double
        | ImmediateBool Bool
        | Binary BinaryOp Expr Expr     
        | Unary UnaryOp Expr         -- E.g. sqrt(a)
        | Tuple (NonEmpty Expr)      -- E.g. (a, b, c) 
    deriving (Show, Eq)

-- | An annotation attached to a block for LaTeX output.
-- The elaboration phase ensures there are no duplicate annotations.
data BlockAnnotation = BlockDisplay BlockDisplayMode | BlockClass String | BlockName String | BlockLabel String | BlockDescription String
    deriving (Show, Eq)

-- | Display mode for a block when rendering to LaTeX.
data BlockDisplayMode   = DefaultBlock  -- outputs block in \begin{flalign*}...\end{flalign*} block (this can not be parsed, only set by the elaboration phase)
                        | BoxBlock      -- outputs nicely boxed block
                        | InTextBlock   -- outputs block as in-text lines wrapped with $
                        | InLineBlock   -- outputs block as a single line wrapped with $
                        | HiddenBlock   -- omits the block from LaTeX output
    deriving (Show, Eq)

-- | Annotation attached to a declaration for LaTeX output.
-- The elaboration phase ensures there are no duplicate annotations.
data DeclAnnotation = DeclDisplay DeclDisplayMode
    deriving (Show, Eq)

-- | Display mode for a declaration when rendering to LaTeX.
data DeclDisplayMode    = DefaultDecl   -- declaration is emitted as usual (this can not be parsed, only set by the elaboration phase)
                        | HiddenDecl    -- omits the declaration from LaTeX output
    deriving (Show, Eq)