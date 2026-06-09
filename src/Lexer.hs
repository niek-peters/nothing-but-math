module Lexer (tokenize, runLexer, TokenizeResult) where

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Types (Fragment(..)) 
import Token
import qualified Data.Map as Map

type TokenLexer = Parsec Void String

type TokenizeResult = [Fragment String [Token] [Token]]


-- Splitting the source code into sections

tokenize :: String -> TokenizeResult
tokenize str = case parse sectionSplitter "" str of
  Left err -> error $ errorBundlePretty err
  Right res -> res

sectionSplitter :: TokenLexer TokenizeResult
sectionSplitter = manyTill (codeBlock <|> evalBlock <|> textBlock) eof
  where
    codeBlock = try (symbol "<<<") *> (DefinitionFragment <$> lexUntil ">>>")
    evalBlock = try (symbol "{{{") *> (EvalFragment <$> lexUntil "}}}")
    textBlock = TextFragment . concat <$> some (commentStr <|> normalTextLine)

    commentStr = (\start (body, end) -> start ++ body ++ end) <$> string "%" <*> manyTill_ anySingle (string "\n" <|> ("" <$ eof))
    
    normalTextLine = some (
        notFollowedBy (string "%")
        *> notFollowedBy (try (string "<<<"))
        *> notFollowedBy (try (string "{{{"))
        *> anySingle
      )

lexUntil :: String -> TokenLexer [Token]
lexUntil endMarker = manyTill lexer (try $ string endMarker)

whitespace :: TokenLexer ()
whitespace = L.space space1 (L.skipLineComment "%") empty

lexeme :: TokenLexer a -> TokenLexer a
lexeme = L.lexeme whitespace

symbol :: String -> TokenLexer String
symbol = L.symbol whitespace

-- Order-sensitive symbol table
symbolTable :: [(String, Token)]
symbolTable = [ 
  ("Z+", TPrimType Positive),   -- we treat Z+ as a symbol as it cannot be part of an identifier name anyway
  (":=", TAssign),
  ("->", TArrow),
  ("/=", TBOp Neq),
  ("<=", TBOp LessEq),
  (">=", TBOp GreaterEq),
  (":" , TColon),
  ("," , TComma),
  ("#" , THash),
  ("@" , TAt),
  ("=" , TBOp Eq),
  ("(" , TLParen),
  (")" , TRParen),
  ("[" , TLBracket),
  ("]" , TRBracket),
  ("{" , TLBrace),
  ("}" , TRBrace),
  ("+" , TBOp Add),
  ("-" , TMinus),   -- minus is ambiguous, could be either UOp or BOp
  ("*" , TBOp Mult),
  ("/" , TBOp Div),
  ("^" , TBOp Pow),
  ("<" , TBOp Less),
  (">" , TBOp Greater),
  ("|" , TBOp Divides)
  ]

keywordTable :: Map.Map String Token
keywordTable = Map.fromList [ 
  ("not", TUOp Not),
  ("and", TBOp And),
  ("or", TBOp Or),
  ("mod", TBOp Mod),
  ("if", TIf),
  ("otherwise", TOtherwise),
  ("where", TWhere),
  ("True", TBool True),
  ("False", TBool False),
  ("sqrt", TUOp Sqrt),
  ("floor", TUOp Floor),
  ("N", TPrimType Natural),
  ("Z", TPrimType Integer),
  ("Q", TPrimType Rational),
  ("R", TPrimType Real),
  ("B", TPrimType Boolean)
  ]

runLexer :: String -> [Token]
runLexer str = case parse (manyTill lexer eof) "" str of
  Left err -> error $ errorBundlePretty err
  Right res -> res

lexer :: TokenLexer Token
lexer = lexeme (
        try (TReal <$> L.float)
    <|> TInt <$> L.decimal
    <|> stringLiteralLexer
    <|> symbolLexer
    <|> wordLexer
  )

stringLiteralLexer :: TokenLexer Token
stringLiteralLexer = lexeme $ TStr <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- | Maps the symbolTable directly to a choice list of symbol parsers, using the order in which they are listed above.
symbolLexer :: TokenLexer Token
symbolLexer = choice $ map (\(str, t) -> try (t <$ symbol str)) symbolTable

-- | Eats an alphanumeric string and checks if it's a known keyword.
-- If not, it treats it as an identifier
wordLexer :: TokenLexer Token
wordLexer = lexeme $ do
  str <- (:) <$> (lowerChar <|> upperChar) <*> many alphaNumChar  -- starts with a character
  pure $ case Map.lookup str keywordTable of
    Just tok -> tok
    Nothing  -> TId str