module Parser (Parser.parse, ParseResult, runExprParser) where

import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Set as Set
import AST
import Data.List.NonEmpty (fromList, NonEmpty ((:|)))
import Control.Monad.Combinators.Expr
import Types

-- TODO: make section parser ignore <<<, >>>, {{{ and }}} if there is a % before it on the same line
-- TODO: consider enforcing newlines in many places

type Parser = Parsec Void String

type ParseResult = [Fragment String AST Expr]
type SplitResult = [Fragment String String String]

whitespace :: Parser ()
whitespace = Lexer.space space1 (Lexer.skipLineComment "%") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

symbol :: String -> Parser String
symbol = Lexer.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

keywords :: Set.Set String
keywords = Set.fromList ["if", "otherwise", "where", "True", "False", "Z+", "Z", "N", "Q", "R", "B", "not", "and", "or"]    -- TODO: make it impossible to declare floor/sqrt/mod, but still allow the first two in function calls

identifier :: Parser String
identifier = (lexeme . try) $ do
  name <- (:) <$> lowerChar <*> many alphaNumChar
  if Set.member name keywords
    then fail $ "Cannot use keyword " ++ show name ++ " as an identifier"
    else return name

parse :: String -> ParseResult
parse str = map runFragmentParser sections
    where sections = runSectionsParser str

useParser :: Parser a -> String -> a
useParser parser str = case Text.Megaparsec.parse parser "" str of
    Left e -> error $ show e
    Right r -> r

runSectionsParser :: String -> SplitResult
runSectionsParser = useParser parseSections

runExprParser :: String -> Expr
runExprParser = useParser parseExpr

parseSections :: Parser SplitResult
parseSections = manyTill (choice [codeStr, evalStr, textStr]) eof
  where
    codeStr = DefinitionFragment <$> (try (string "<<<") *> manyTill anySingle (string ">>>"))
    evalStr = EvalFragment <$> (try (string "{{{") *> manyTill anySingle (string "}}}"))
    textStr = TextFragment <$> some (
        notFollowedBy (string "<<<") 
        *> notFollowedBy (string "{{{") 
        *> anySingle
      )

runFragmentParser :: Fragment String String String -> Fragment String AST Expr
runFragmentParser (TextFragment t) = TextFragment t
runFragmentParser (DefinitionFragment c) = DefinitionFragment $ useParser parseCodeFragment c
runFragmentParser (EvalFragment e) = EvalFragment $ useParser (whitespace *> lexeme parseExpr <* eof) e

parseCodeFragment :: Parser AST
parseCodeFragment = AST <$> (whitespace *> (lexeme $ option [] (try $ symbol "#" *> brackets (sepBy parseBlockAnnotation (symbol ","))))) <*> (whitespace *> (lexeme $ many (lexeme parseDeclaration)) <* eof)

parseDeclaration :: Parser Declaration
parseDeclaration = do
    declAns <- option [] (try $ symbol "@" *> brackets (sepBy parseDeclAnnotation (symbol ",")))
    name <- identifier  -- we parse the name and later ensure it comes up again for the implementation

    Declaration 
        <$> pure declAns
        <*> pure name 
        <*> (symbol ":" *> parseSignature) 
        <*> (symbol name *> (option [] (parens (sepBy identifier (symbol ",")))))
        <*> (symbol ":=" *> parseImplementation)
        <*> option [] (try $ symbol "where" *> (parseWherePart))
    where
        parseWherePart = sepBy1 (LocalDecl <$> try parseLocal <|> Constraint <$> parseExpr) (symbol ",")

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
                    <|> Boolean <$ symbol "B"

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
      binaryR "^"       Pow
    ], 
    [
      unary   "-"       Neg,
      reservedUOp "not" Not
    ],
    [ 
      binary  "*"       Mult, 
      binaryNotFollowed  "/" Div (char '='), 
      reservedBOp "mod"  Mod 
    ], 
    [ 
      binary  "+"       Add, 
      binary  "-"       Sub 
    ], 
    [ 
      binary  "="       Eq, 
      binary  "/="      Neq, 
      binary  "<="      LessEq, 
      binary  ">="      GreaterEq, 
      binary  "<"       Less, 
      binary  ">"       Greater, 
      binary  "|"       Divides
    ],
    [
      reservedBOp "and"  And
    ],
    [
      reservedBOp "or"  Or
    ]
  ]
  where
    unary s cons = Prefix (Unary cons <$ symbol s)
    binary s cons = InfixL (Binary cons <$ symbol s)
    binaryNotFollowed s cons notFollowed = InfixL (Binary cons <$ try (symbol s <* notFollowedBy notFollowed))   -- this ensures it does not eat anything parsed by 'notFollowed'
    binaryR s cons = InfixR (Binary cons <$ symbol s)  -- right-associative
    reservedUOp s cons = Prefix (Unary cons <$ (lexeme . try) (string s <* notFollowedBy alphaNumChar))  -- this ensures it does not eat identifiers starting with the name of some operator. E.g. a function called notOne
    reservedBOp s cons = InfixL (Binary cons <$ (lexeme . try) (string s <* notFollowedBy alphaNumChar))  -- this ensures it does not eat identifiers starting with the name of some operator. E.g. a function called modInv

parseTerm :: Parser Expr
parseTerm = parens (try parseTuple <|> parseExpr)
  <|> parseCall
  <|> try (ImmediateReal <$> lexeme Lexer.float)
  <|> ImmediateInt  <$> lexeme Lexer.decimal
  <|> ImmediateBool <$> (True <$ symbol "True" <|> False <$ symbol "False")

-- NOTE: we explicitly disallow single-element tuples here
parseTuple :: Parser Expr
parseTuple = Tuple <$> ((:|) <$> (parseExpr <* symbol ",") <*> sepBy1 parseExpr (symbol ","))

parseCall :: Parser Expr
parseCall = do
  name <- identifier
  maybeArgs <- optional (parens (sepBy parseExpr (symbol ",")))
  
  return $ case (name, maybeArgs) of
    (n, Nothing)     -> Call n []         -- value reference
    ("floor", Just [e]) -> Unary Floor e  -- handle special built-in operations that are written like function calls in the DSL
    ("sqrt",  Just [e])  -> Unary Sqrt e
    (n, Just args)   -> Call n args       -- function call

parseBlockAnnotation :: Parser BlockAnnotation
parseBlockAnnotation = try (BlockDisplay <$> parseBlockDisplay)
                      <|> parseKeyValue
  where 
    parseKeyValue = do
      (key, value) <- (,) <$> identifier <* symbol "=" <*> lexeme (manyTill anySingle (lookAhead (symbol "," <|> symbol "]")))
      let attr = case key of
            "class" -> BlockClass
            "name" -> BlockName
            "label" -> BlockLabel
            "description" -> BlockDescription
            _ -> error $ "SYNTAX ERROR: Invalid attribute name '" ++ key ++ "'"
      return $ attr value

parseBlockDisplay :: Parser BlockDisplayMode
parseBlockDisplay = DefaultBlock <$ symbol "default"
                    <|> BoxBlock <$ symbol "box"
                    <|> try (InTextBlock <$ symbol "intext")
                    <|> try (InLineBlock <$ symbol "inline")
                    <|> HiddenBlock <$ symbol "hidden"

parseDeclAnnotation :: Parser DeclAnnotation
parseDeclAnnotation = try (DeclDisplay <$> parseDeclDisplay)

parseDeclDisplay :: Parser DeclDisplayMode
parseDeclDisplay = try (DefaultDecl <$ symbol "default")
                    <|> HiddenDecl <$ symbol "hidden"
