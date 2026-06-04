module IR (module IR) where -- export everything

import AST (Id, Signature, UnaryOp, PrimitiveType, BlockDisplayMode (DefaultBlock), DeclDisplayMode (DefaultDecl))
import Data.List.NonEmpty (NonEmpty)

data IR = IR IRBlockAnnotations [IRDeclaration]
    deriving (Show, Eq)

data IRDeclaration = IRDeclaration 
    IRDeclAnnotations
    Id              -- name
    Signature       -- type signature
    [Id]            -- arguments
    IRImplementation  
    [IRWhereTerm]
    deriving (Show, Eq)

data IRImplementation   = IRUnconditional IRExpr 
                        | IRConditional [IRBranch] IRExpr -- piecewise function. Expr is otherwise branch
    deriving (Show, Eq)

data IRBranch = IRBranch IRExpr IRExpr  -- Expr if Expr
    deriving (Show, Eq)

data IRWhereTerm = IRLocalDecl IRLocal | IRConstraint IRExpr
    deriving (Show, Eq)

data IRLocal = IRLocal 
    (NonEmpty Id)   -- singular usually, but also allows tuple destructuring, e.g. (a, b) := f(x)
    IRExpr  
    deriving (Show, Eq)

data IRExpr = IRCast IRExpr PrimitiveType PrimitiveType       -- wraps an expr that should be cast from type to type
            | IRCall Id Bool [IRExpr]       -- boolean indicates whether the reference is to a global identifier
            | IRImmediateInt Int PrimitiveType  -- the PrimitiveType indicates whether this immediate value should be interpreted as Positive, Natural or Integer
            | IRImmediateReal Double
            | IRImmediateBool Bool
            | IRBinary IRBinaryOp IRExpr IRExpr     
            | IRUnary UnaryOp IRExpr         -- E.g. sqrt(a)
            | IRTuple (NonEmpty IRExpr)      -- E.g. (a, b, c) 
    deriving (Show, Eq)

-- here we differentiate between:
-- 1. Frac (Haskell %, creates a fraction) and Div (Haskell /, performs a division)
-- 2. Pow (Haskell ^, integer power) and Exp (Haskell **, rational/real power)
data IRBinaryOp = IRAdd | IRSub | IRMult | IRFrac | IRDiv | IRPow | IRExp | IRMod | IREq | IRNeq | IRLess | IRGreater | IRLessEq | IRGreaterEq | IRDivides
    deriving (Show, Eq)


-- in the IR we explicitly have one value for each possible annotation setting
data IRBlockAnnotations = IRBlockAnnotations 
    {blockDisplayMode :: BlockDisplayMode, blockName :: Maybe String, blockLabel :: Maybe String, blockClass :: String}
    deriving (Show, Eq)

defaultBlockAnnotations :: IRBlockAnnotations
defaultBlockAnnotations = IRBlockAnnotations {blockDisplayMode = DefaultBlock, blockName = Nothing, blockLabel = Nothing, blockClass = "Definition"}

data IRDeclAnnotations = IRDeclAnnotations 
    {declDisplayMode :: DeclDisplayMode}
    deriving (Show, Eq)

defaultDeclAnnotations :: IRDeclAnnotations
defaultDeclAnnotations = IRDeclAnnotations {declDisplayMode = DefaultDecl}