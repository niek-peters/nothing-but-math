module Token (module Token) where

data Token
  = TId String
  | TStr String   -- string literals used for annotation values to allow arbitrary characters
  | TInt Int
  | TReal Double
  | TBool Bool
  -- Operators
  | TUOp UnaryOp    -- excludes Neg
  | TBOp BinaryOp   -- excludes Sub
  -- Ambiguous operators
  | TMinus
  -- Keywords
  | TIf 
  | TOtherwise 
  | TWhere
  -- Primitive types
  | TPrimType PrimitiveType
  -- Special symbols
  | TColon 
  | TColonEq 
  | TArrow 
  | TComma 
  | THash 
  | TAt 
  | TEqualSign
  | TLParen 
  | TRParen 
  | TLBracket 
  | TRBracket 
  | TLBrace 
  | TRBrace
  -- Text elements outside code/eval blocks
  | TTextString String
  deriving (Show, Eq)

data BinaryOp = Add | Sub | Mult | Div | Pow | Mod | Eq | Neq | Less | Greater | LessEq | GreaterEq | Divides | And | Or
    deriving (Show, Eq)

data UnaryOp = Neg | Sqrt | Floor | Not
    deriving (Show, Eq)

data PrimitiveType = Positive | Natural | Integer | Rational | Real | Boolean
    deriving (Show, Eq)

data BlockDisplayMode   = DefaultBlock  -- outputs block in \begin{flalign*}...\end{flalign*} block
                        | BoxBlock      -- outputs nicely boxed block
                        | InTextBlock   -- outputs block as in-text lines wrapped with $
                        | InLineBlock   -- outputs block as a single line wrapped with $
                        | HiddenBlock   -- omits the block from LaTeX output
    deriving (Show, Eq)

data DeclDisplayMode    = DefaultDecl   -- declaration is emitted as usual
                        | HiddenDecl    -- omits the declaration from LaTeX output
    deriving (Show, Eq)