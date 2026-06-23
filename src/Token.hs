{-# LANGUAGE FlexibleInstances #-}

-- | Token definitions and stream instances used by the lexer and parser.
module Token (module Token) where

import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as M

import qualified Data.List.NonEmpty as NonEmpty
import Data.List (intercalate)

-- | Tokens produced by the lexer and consumed by the parser.
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

-- | Binary operators supported by the language grammar.
data BinaryOp = Add | Sub | Mult | Div | Pow | Mod | Eq | Neq | Less | Greater | LessEq | GreaterEq | Divides | And | Or
    deriving (Show, Eq, Ord)

-- | Unary operators supported by the language grammar.
data UnaryOp = Neg | Sqrt | Floor | Not
    deriving (Show, Eq, Ord)

-- | Primitive numeric and boolean types supported by the language.
data PrimitiveType = Positive | Natural | Integer | Rational | Real | Boolean
    deriving (Show, Eq)

-- | Order primitive numeric types by widening rank, with Boolean excluded from ordering.
-- This allows us to easily determine whether a number type is a subtype of another number type,
-- also giving us access to the min and max functions.
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

-- For transparency: the pretty printing code below was AI-generated

-- | Pretty-print token streams in parser error messages.
instance M.VisualStream [Token] where
    showTokens _ chunks = unwords (map show (NonEmpty.toList chunks))
    tokensLength _ chunks = length chunks
  
-- | Track source position while traversing token streams during parsing.
instance TraversableStream [Token] where
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