module Parser (Parser.parse, ParseResult(..), Fragment(..)) where

import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Set as Set
import AST
import Data.Either (partitionEithers)
import Data.List.NonEmpty (fromList)
import Control.Monad.Combinators.Expr

-- TODO: consider enforcing newlines in many places

type Parser = Parsec Void String

newtype ParseResult = ParseResult [Fragment]
    deriving (Show, Eq)
data Fragment = TextFragment String | CodeFragment AST
    deriving (Show, Eq)

newtype SplitResult = SplitResult [Section]
    deriving (Show, Eq)
data Section = TextSection String | CodeSection String
    deriving (Show, Eq)

whitespace :: Parser ()
whitespace = Lexer.space space1 (Lexer.skipLineComment "%") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

symbol :: String -> Parser String
symbol = Lexer.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keywords :: Set.Set String
keywords = Set.fromList ["if", "otherwise", "where", "True", "False", "Z+", "Z", "N", "Q", "R", "B"]    -- TODO: make it impossible to declare floor/sqrt/mod, but still allow the first two in function calls

identifier :: Parser String
identifier = (lexeme . try) $ do
  name <- (:) <$> lowerChar <*> many alphaNumChar
  if Set.member name keywords
    then fail $ "Cannot use keyword " ++ show name ++ " as an identifier"
    else return name

parse :: String -> ParseResult
parse str = ParseResult $ map runFragmentParser sections
    where (SplitResult sections) = runSectionsParser str

runSectionsParser :: String -> SplitResult
runSectionsParser str = case Text.Megaparsec.parse parseSections "" str of
    Left e -> error $ show e
    Right r -> r

parseSections :: Parser SplitResult
parseSections = SplitResult <$> manyTill (choice [codeStr, textStr]) eof
  where
    codeStr = CodeSection <$> (try (string "<<<") *> manyTill anySingle (string ">>>"))
    textStr = TextSection <$> some (notFollowedBy (string "<<<") *> anySingle)

runFragmentParser :: Section -> Fragment
runFragmentParser (TextSection t) = TextFragment t
runFragmentParser (CodeSection c) = case Text.Megaparsec.parse parseCodeFragment "" c of
            Left e -> error $ show e
            Right r -> CodeFragment r

parseCodeFragment :: Parser AST
parseCodeFragment = AST <$> (whitespace *> (lexeme $ many (lexeme parseDeclaration)) <* eof)

parseDeclaration :: Parser Declaration
parseDeclaration = do
    name <- identifier  -- we parse the name and later ensure it comes up again for the implementation

    base <- Declaration name 
        <$> (symbol ":" *> parseSignature) 
        <*> (symbol name *> (option [] (parens (sepBy identifier (symbol ",")))))
        <*> (symbol ":=" *> parseImplementation)
        
    (locals, constraints) <- option ([], []) (try $ symbol "where" *> (parseWherePart))

    return $ base locals constraints
    where
        parseWherePart = partitionEithers <$> sepBy1 (Left <$> try parseLocal <|> Right <$> parseExpr) (symbol ",")

parseSignature :: Parser Signature
parseSignature = toSignature <$> parseType <*> optional (symbol "->" *> parseType)
    where
        toSignature t1 Nothing   = Signature Nothing t1     -- there is no '->'. This is a constant
        toSignature t1 (Just t2) = Signature (Just t1) t2   

parseType :: Parser Type
parseType = Type . fromList <$> sepBy1 parsePrimitiveType (symbol "x")

parsePrimitiveType :: Parser PrimitiveType
parsePrimitiveType = Positive <$ symbol "Z+"
                    <|> Natural <$ symbol "N"
                    <|> Integer <$ symbol "Z"
                    <|> Rational <$ symbol "Q"
                    <|> Real <$ symbol "R"

parseImplementation :: Parser Implementation
parseImplementation = between (symbol "{") (symbol "}") parseConditional <|> Unconditional <$> parseExpr
    where
        parseConditional = Conditional <$> many (try parseBranch) <*> (parseExpr <* symbol "otherwise")     -- TODO: consider not using many but something that requires at least 1
        parseBranch = Branch <$> parseExpr <*> (symbol "if" *> parseExpr)

-- NOTE: for now locals are unconditional
parseLocal :: Parser Local
parseLocal = Local <$> parseLHS <* symbol ":=" <*> parseExpr
  where
    parseLHS = fromList <$> (parens (sepBy1 identifier (symbol ",")) <|> pure <$> identifier)   -- LHS is either a single id or a parenthesized tuple of ids

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm exprTable

exprTable :: [[Operator Parser Expr]]
exprTable = [
    [ 
      binaryR "^"    (Binary Pow) 
    ], 
    [ 
      binary  "*"    (Binary Mult), 
      binary  "/"    (Binary Div), 
      reservedOp "mod" (Binary Mod) 
    ], 
    [ 
      binary  "+"    (Binary Add), 
      binary  "-"    (Binary Sub) 
    ], 
    [ 
      binary  "="    (Binary Eq), 
      binary  "!="   (Binary Neq), 
      binary  "<="   (Binary LessEq), 
      binary  ">="   (Binary GreaterEq), 
      binary  "<"    (Binary Less), 
      binary  ">"    (Binary Greater), 
      binary  "|"    (Binary Divides)
    ]
  ]
  where
    binary  name f = InfixL (f <$ symbol name)
    binaryR name f = InfixR (f <$ symbol name)  -- right-associative
    reservedOp name f = InfixL (f <$ (lexeme . try) (string name <* notFollowedBy alphaNumChar))  -- this ensures it does not eat identifiers starting with the name of some operator. E.g. a function called modInv

parseTerm :: Parser Expr
parseTerm = try parseTuple
  <|> parens parseExpr
  <|> parseCall
  <|> try (ImmediateReal <$> lexeme Lexer.float)
  <|> ImmediateInt  <$> lexeme Lexer.decimal
  <|> ImmediateBool <$> (True <$ symbol "True" <|> False <$ symbol "False")

parseTuple :: Parser Expr
parseTuple = Tuple . fromList <$> parens (sepBy1 parseExpr (symbol ","))

parseCall :: Parser Expr
parseCall = do
  name <- identifier
  maybeArgs <- optional (parens (sepBy parseExpr (symbol ",")))
  
  return $ case (name, maybeArgs) of
    (n, Nothing)     -> Call n []         -- value reference
    ("floor", Just [e]) -> Unary Floor e  -- handle special built-in operations that are written like function calls in the DSL
    ("sqrt",  Just [e])  -> Unary Sqrt e
    (n, Just args)   -> Call n args       -- function call