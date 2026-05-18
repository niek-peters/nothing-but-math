module Parser (Parser.parse) where

import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Set as Set
import AST
import Data.Either (partitionEithers)
import Data.List.NonEmpty (fromList)
import Control.Monad.Combinators.Expr
import Control.Monad (void)

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
keywords = Set.fromList ["if", "otherwise", "where", "True", "False", "Z+", "Z", "N", "Q", "R", "B"]

identifier :: Parser String
identifier = (lexeme . try) $ do
  name <- (:) <$> lowerChar <*> many alphaNumChar
  if Set.member name keywords
    then fail $ "Cannot use keyword " ++ show name ++ " as an identifier"
    else return name

-- testParseSections :: String -> SplitResult
-- testParseSections str = case Text.Megaparsec.parse parseSections "" str of
--     Left e -> error $ show e
--     Right r -> r

-- parse :: String -> ParseResult
-- parse str = case Text.Megaparsec.parse parseSections "" str of
--     Left e -> error $ show e
--     Right (SplitResult r) -> ParseResult $ map refine r
--     where
--         refine (TextSection t) = TextFragment t
--         refine (CodeSection c) = case Text.Megaparsec.parse parseCodeFragment "" c of
--             Left e -> error $ show e
--             Right r -> CodeFragment r

parse :: String -> ParseResult
parse str = ParseResult $ map runFragmentParser sections
    where (SplitResult sections) = runSectionsParser str
    
runSectionsParser :: String -> SplitResult
runSectionsParser str = case Text.Megaparsec.parse parseSections "" str of
    Left e -> error $ show e
    Right r -> r

runFragmentParser :: Section -> Fragment
runFragmentParser (TextSection t) = TextFragment t
runFragmentParser (CodeSection c) = case Text.Megaparsec.parse parseCodeFragment "" c of
            Left e -> error $ show e
            Right r -> CodeFragment r

parseCodeFragment :: Parser AST
parseCodeFragment = AST <$> many parseDeclaration

-- parseLibrary :: Parser Library
-- parseLibrary = whitespace *> (Library <$> concat <$> manyTill (try codeBlock <|> text) eof)
--   where
--     codeBlock = symbol "<<<" *> manyTill parseDeclaration (symbol ">>>")  -- only parse within code blocks
--     text = anySingle *> pure []         -- ignore text


-- parseFull :: Parser ParseResult
-- parseFull = do
--     sections <- splitSections
--     return 


parseSections :: Parser SplitResult
parseSections = SplitResult <$> manyTill (choice [codeStr, textStr]) eof
  where
    codeStr = CodeSection <$> (try (string "<<<") *> manyTill anySingle (string ">>>"))
    textStr = TextSection <$> some (notFollowedBy (string "<<<") *> anySingle)

-- parseLibrary :: Parser Library
-- parseLibrary = Library <$> manyTill parseFragment eof

-- parseFragment :: Parser Fragment
-- parseFragment = try parseCodeBlock <|> parseTextBlock

-- parseCodeBlock :: Parser Fragment
-- parseCodeBlock = Code <$> (symbol "<<<" *> manyTill (parseDeclaration <* whitespace) (symbol ">>>"))

-- parseTextBlock :: Parser Fragment
-- parseTextBlock = Text <$> some (notFollowedBy (string "<<<") *> anySingle) 



parseDeclaration :: Parser Declaration
parseDeclaration = do
    name <- identifier  -- we parse the name and later ensure it comes up again for the implementation

    base <- Declaration name 
        <$> (symbol ":" *> parseSignature) 
        <*> (symbol name *> (option [] (parens (sepBy identifier (symbol ",")))))
        <*> (symbol ":=" *> parseImplementation)
        
    (locals, constraints) <- option ([], []) (symbol "where" *> parseWherePart)
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
        parseConditional = Conditional <$> many (try parseBranch) <*> (symbol "otherwise" *> parseExpr)     -- TODO: consider not using many but something that requires at least 1
        parseBranch = Branch <$> parseExpr <*> (symbol "if" *> parseExpr)

parseLocal :: Parser Local
parseLocal = Local <$> parseLHS <* symbol ":=" <*> parseImplementation
  where
    parseLHS = fromList <$> (parens (sepBy1 identifier (symbol ",")) <|> pure <$> identifier)   -- LHS is either a single id or a parenthesized tuple of ids

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm exprTable

exprTable :: [[Operator Parser Expr]]
exprTable =
  [ [ binaryR "^"    (Binary Pow) ]
  , [ binary  "*"    (Binary Mult)
    , binary  "/"    (Binary Div)
    , binary  "mod"  (Binary Mod) 
    ]
  , [ binary  "+"    (Binary Add)
    , binary  "-"    (Binary Sub) 
    ]
  , [ binary  "="    (Binary Eq)
    , binary  "!="   (Binary Neq)
    , binary  "<="   (Binary LessEq)
    , binary  ">="   (Binary GreaterEq)
    , binary  "<"    (Binary Less)
    , binary  ">"    (Binary Greater)
    , binary  "|"    (Binary Divides)
    ]
  ]
  where
    binary  name f = InfixL (f <$ symbol name)
    binaryR name f = InfixR (f <$ symbol name)  -- right-associative

parseTerm :: Parser Expr
parseTerm = try parseTuple
  <|> parens parseExpr
  <|> parseCall
  <|> try (ImmediateReal <$> lexeme Lexer.float)
  <|> ImmediateInt  <$> lexeme Lexer.decimal
  <|> ImmediateBool <$> (True <$ symbol "True" <|> False <$ symbol "False")

parseTuple :: Parser Expr
parseTuple = Tuple <$> parens (sepBy1 parseExpr (symbol ","))

parseCall :: Parser Expr
parseCall = do
  name <- identifier
  -- Check if there's a '(' immediately following the name
  maybeArgs <- optional (parens (sepBy parseExpr (symbol ",")))
  
  return $ case (name, maybeArgs) of
    -- It was a variable reference: f
    (n, Nothing)     -> Call n []
    
    -- It was a function call: f(...)
    -- Now we handle your special unary cases
    ("floor", Just [e]) -> Unary Floor e
    ("sqrt",  Just [e])  -> Unary Sqrt e
    
    -- It was a standard function call: gcd(a, b)
    (n, Just args)   -> Call n args