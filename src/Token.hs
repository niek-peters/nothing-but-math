{-# LANGUAGE FlexibleInstances #-}

module Token (module Token) where

import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as M

import qualified Data.List.NonEmpty as NonEmpty
import Data.List (intercalate)

data Token
  = TId String
  | TStr String   -- string literals used for annotation values to allow arbitrary characters
  | TInt Integer
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
  | TAssign 
  | TArrow 
  | TComma 
  | THash 
  | TAt 
  | TLParen 
  | TRParen 
  | TLBracket 
  | TRBracket 
  | TLBrace 
  | TRBrace
  -- Text elements outside code/eval blocks
  | TTextString String
  deriving (Show, Eq, Ord)

data BinaryOp = Add | Sub | Mult | Div | Pow | Mod | Eq | Neq | Less | Greater | LessEq | GreaterEq | Divides | And | Or
    deriving (Show, Eq, Ord)

data UnaryOp = Neg | Sqrt | Floor | Not
    deriving (Show, Eq, Ord)

data PrimitiveType = Positive | Natural | Integer | Rational | Real | Boolean
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

-- | 1. Tell Megaparsec how to step through a list of your Tokens
-- instance M.Stream [Token] where
--   type Token  [Token] = Token
--   type Tokens [Token] = [Token]

--   tokenToChunk  _ t   = [t]
--   tokensToChunk _ ts  = ts
--   chunkToTokens _ ts  = ts
--   chunkLength   _ ts  = length ts
--   chunkEmpty    _ ts  = null ts
  
--   take1_ []       = Nothing
--   take1_ (t:ts)   = Just (t, ts)
  
--   takeN_ n ts | n <= 0    = Just ([], ts)
--               | null ts   = Nothing
--               | otherwise = let (chunk, rest) = splitAt n ts 
--                             in Just (chunk, rest)
                            
--   takeWhile_ f ts = span f ts

-- | 2. Tell Megaparsec how to format your Tokens visually when errors occur
instance M.VisualStream [Token] where
  showTokens _ chunks = unwords (map show (NonEmpty.toList chunks))
  tokensLength _ chunks = length chunks
  
-- | 2. Tell Megaparsec how to track line positions on a token array
instance TraversableStream [Token] where
  -- reachOffset offset state =  (Just streamString, state { M.pstateOffset = offset })
  --   where remainingTokens = M.pstateInput state
  --         first10Tokens = take 10 remainingTokens
  --         streamString = unwords (map show first10Tokens) ++ (if length remainingTokens > 10 then "..." else "")
  reachOffset offset state = (Just streamString, state  { M.pstateOffset = offset, M.pstateSourcePos = (M.pstateSourcePos state) { M.sourceColumn = M.mkPos caretColumn }})
    where 
          currentInput = M.pstateInput state
          consumedCount = offset - M.pstateOffset state
          (allContextTokens, errorAndBeyond) = splitAt consumedCount currentInput
          
          -- cap the context to at most the last 3 tokens
          contextTokens = drop (length allContextTokens - 3) allContextTokens
          hasHiddenContext = length allContextTokens > 3
          
          -- build the snapshot (3 past tokens + 10 future tokens)
          snapshotTokens = contextTokens ++ take 10 errorAndBeyond
          
          -- construct the final display string with smart ellipsis bounds
          prefixStr    = if hasHiddenContext then "... " else ""
          suffixStr    = if length errorAndBeyond > 10 then " ..." else ""
          streamString = prefixStr ++ intercalate ", " (map show snapshotTokens) ++ suffixStr
          
          -- calculate the caret column, accounting for the prefix if it's there
          prefixWidth  = if hasHiddenContext then length prefixStr else 0
          contextWidth = if null contextTokens 
                         then 0 
                         else sum (map (length . show) contextTokens) + 2 * length contextTokens
                         
          caretColumn  = prefixWidth + contextWidth + 1

  reachOffsetNoLine offset state =  state { M.pstateOffset = offset }
    
-- -- | Clean visual representation of tokens for error strings
-- prettyShowToken :: Token -> String
-- prettyShowToken t = case t of
--   TId name        -> name
--   TStr text       -> "\"" ++ text ++ "\""
--   TInt val        -> show val
--   TReal val       -> show val
--   TBool True      -> "True"
--   TBool False     -> "False"
--   TUOp op         -> prettyShowUOp op
--   TBOp op         -> prettyShowBOp op
--   TMinus          -> "-"
--   TIf             -> "if"
--   TOtherwise      -> "otherwise"
--   TWhere          -> "where"
--   TPrimType pt    -> prettyShowPrim pt
--   TColon          -> ":"
--   TAssign         -> ":="
--   TArrow          -> "->"
--   TComma          -> ","
--   THash           -> "#"
--   TAt             -> "@"
--   TLParen         -> "("
--   TRParen         -> ")"
--   TLBracket       -> "["
--   TRBracket       -> "]"
--   TLBrace         -> "{"
--   TRBrace         -> "}"
--   TTextString s   -> s
--   where
--     prettyShowBOp Add = "+"; prettyShowBOp Sub = "-"; prettyShowBOp Mult = "*"; prettyShowBOp Div = "/"
--     prettyShowBOp Pow = "^"; prettyShowBOp Mod = "mod"; prettyShowBOp Eq = "="; prettyShowBOp Neq = "/="
--     prettyShowBOp Less = "<"; prettyShowBOp Greater = ">"; prettyShowBOp LessEq = "<="; prettyShowBOp GreaterEq = ">="
--     prettyShowBOp Divides = "|"; prettyShowBOp And = "and"; prettyShowBOp Or = "or"

--     prettyShowUOp Neg = "-"; prettyShowUOp Sqrt = "sqrt"; prettyShowUOp Floor = "floor"; prettyShowUOp Not = "not"

--     prettyShowPrim Positive = "Z+"; prettyShowPrim Natural = "N"; prettyShowPrim Integer = "Z"
--     prettyShowPrim Rational = "Q";  prettyShowPrim Real = "R";    prettyShowPrim Boolean = "B"