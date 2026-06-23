-- | Intermediate representation used after elaboration and before code generation.
module IR (module IR) where -- export everything

import AST (Id, Signature, BlockDisplayMode (DefaultBlock), DeclDisplayMode (DefaultDecl))
import Data.List.NonEmpty (NonEmpty)
import Token (PrimitiveType, UnaryOp)

-- | Complete IR for code block consisting of block annotations and declarations.
data IR = IR IRBlockAnnotations [IRDeclaration]
    deriving (Show, Eq)

-- | Elaborated declaration with IR annotations, signature, body, and where-terms.
data IRDeclaration = IRDeclaration 
    IRDeclAnnotations
    Id              -- name
    Signature       -- type signature
    [Id]            -- arguments
    IRImplementation  
    [IRWhereTerm]
    deriving (Show, Eq)

-- | Elaborated declaration body, either unconditional or guarded by branches.
data IRImplementation   = IRUnconditional IRExpr 
                        | IRConditional [IRBranch] IRExpr -- piecewise function. Expr is otherwise branch
    deriving (Show, Eq)

-- | Single IR branch: result expression with its condition.
data IRBranch = IRBranch IRExpr IRExpr  -- Expr if Expr
    deriving (Show, Eq)

-- | IR where-term: either a local declaration or a boolean constraint.
data IRWhereTerm = IRLocalDecl IRLocal | IRConstraint IRExpr
    deriving (Show, Eq)

-- | Elaborated local declaration.
data IRLocal = IRLocal 
    (NonEmpty Id)   -- singular usually, but also allows tuple destructuring, e.g. (a, b) := f(x)
    IRExpr  
    deriving (Show, Eq)

-- | Rendered evaluation result pairing an expression with its computed value.
data IREvalResult = IREvalResult IRExpr IRExpr  -- used to represent an expression and its evaluated result, e.g. f(1, 2) = 3 
    deriving (Show, Eq)                         -- only used by LaTeX code gen

-- | Elaborated expression tree with explicit casts and IR operators.
data IRExpr = IRCast IRExpr PrimitiveType PrimitiveType       -- wraps an expr that should be cast from type to type
            | IRCall Id Bool [IRExpr]       -- boolean indicates whether the reference is to a global identifier
            | IRImmediateInt Integer PrimitiveType  -- the PrimitiveType indicates whether this immediate value should be interpreted as Positive, Natural or Integer
            | IRImmediateReal Double
            | IRImmediateBool Bool
            | IRBinary IRBinaryOp IRExpr IRExpr     
            | IRUnary UnaryOp IRExpr         -- E.g. sqrt(a)
            | IRTuple (NonEmpty IRExpr)      -- E.g. (a, b, c) 
    deriving (Show, Eq)

-- | Binary operators in IR, including explicit variants for division and powers.
-- Here we differentiate between:
-- 1. Frac (Haskell %, creates a fraction) and Div (Haskell /, performs a division).
-- 2. PosPow (Haskell ^, positive/natural exponent), FracPow (Haskell ^^, integer exponent) and FloatPow (Haskell **, rational/real exponent).
data IRBinaryOp = IRAdd | IRSub | IRMult | IRFrac | IRDiv | IRPosPow | IRFracPow | IRFloatPow | IRMod | IREq | IRNeq | IRLess | IRGreater | IRLessEq | IRGreaterEq | IRDivides | IRAnd | IROr
    deriving (Show, Eq)

-- | Fully elaborated block annotations with defaults filled in.
-- Here we explicitly have one value for each possible annotation setting.
data IRBlockAnnotations = IRBlockAnnotations 
    {blockDisplayMode :: BlockDisplayMode, blockName :: Maybe String, blockLabel :: Maybe String, blockClass :: String, blockDescription :: Maybe String}
    deriving (Show, Eq)

-- | Default block annotations used before elaboration fills in explicit values.
defaultBlockAnnotations :: IRBlockAnnotations
defaultBlockAnnotations = IRBlockAnnotations {blockDisplayMode = DefaultBlock, blockName = Nothing, blockLabel = Nothing, blockClass = "Definition", blockDescription = Nothing}

-- | Fully elaborated declaration annotations with defaults filled in.
data IRDeclAnnotations = IRDeclAnnotations 
    {declDisplayMode :: DeclDisplayMode}
    deriving (Show, Eq)

-- | Default declaration annotations used before elaboration fills in explicit values.
defaultDeclAnnotations :: IRDeclAnnotations
defaultDeclAnnotations = IRDeclAnnotations {declDisplayMode = DefaultDecl}