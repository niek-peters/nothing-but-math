-- | Parser for the DSL, turning token streams into ASTs and expressions.
module Parser (Parser.parse, ParseResult, runExprParser) where

import Text.Megaparsec hiding (Token)
import Data.Void (Void)
import qualified Data.Set as Set
import AST
import Data.List.NonEmpty (fromList, NonEmpty ((:|)))
import Control.Monad.Combinators.Expr
import Types
import Token
import Lexer (TokenizeResult)

-- | Parser type specialized to token streams.
type Parser = Parsec Void [Token]

-- | Parsed fragment stream containing text, AST definitions, and expressions.
type ParseResult = [Fragment String AST Expr]

-- | Parse tokenized fragments into AST fragments.
parse :: TokenizeResult -> ParseResult
parse frags = [res | (Just res) <- map runFragmentParser frags] -- here we filter out the empty code/eval fragments

-- | Run a parser on a token stream and raise a descriptive error on failure.
useParser :: Parser a -> [Token] -> a
useParser parser ts = case Text.Megaparsec.parse parser "" ts of
    Left e -> error $ errorBundlePretty e
    Right r -> r

-- | Parse a standalone expression from a token stream.
runExprParser :: [Token] -> Expr
runExprParser = useParser parseExpr

-- | Parse a tokenized fragment into a parsed fragment, discarding empty code/eval blocks.
runFragmentParser :: Fragment String [Token] [Token] -> Maybe (Fragment String AST Expr)
runFragmentParser (TextFragment t) = Just $ TextFragment t
runFragmentParser (DefinitionFragment c) = DefinitionFragment <$> useParser (optional parseCodeFragment <* eof) c
runFragmentParser (EvalFragment e) = EvalFragment <$> useParser (optional parseExpr <* eof) e

-- | Parse a definition block into an AST.
parseCodeFragment :: Parser AST
parseCodeFragment = AST <$> option [] (tok THash *> brackets (sepBy parseBlockAnnotation $ tok TComma)) <*> some parseDeclaration

-- | Parse a single top-level declaration.
parseDeclaration :: Parser Declaration
parseDeclaration = Declaration 
        <$> option [] (tok TAt *> brackets (sepBy parseDeclAnnotation $ tok TComma))
        <*> idTok 
        <*> (tok TColon *> parseSignature) 
        <*> idTok
        <*> (option [] $ parens $ sepBy idTok $ tok TComma)
        <*> (tok TAssign *> parseImplementation)
        <*> option [] (tok TWhere *> parseWherePart)
    where
        parseWherePart = sepBy1 (LocalDecl <$> try parseLocal <|> Constraint <$> parseExpr) $ tok TComma

-- | Parse a declaration signature with optional argument type.
parseSignature :: Parser Signature
parseSignature = toSignature <$> parseType <*> optional (tok TArrow *> parseType)
    where
        toSignature t1 Nothing   = Signature Nothing t1     -- there is no '->'. This is a constant
        toSignature t1 (Just t2) = Signature (Just t1) t2   

-- | Parse a primitive type tuple using `x` as the separator.
parseType :: Parser Type
parseType = Type . fromList <$> sepBy1 primTypeTok (tok $ TId "x")  -- the 'cross'/x symbol is tokenized as an identifier x

-- | Parse an implementation body, either a conditional block or a single expression.
parseImplementation :: Parser Implementation
parseImplementation = braces parseConditional <|> Unconditional <$> parseExpr
    where
        parseConditional = Conditional <$> many (try parseBranch) <*> (parseExpr <* tok TOtherwise)     -- TODO: consider not using many but something that requires at least 1
        parseBranch = Branch <$> parseExpr <*> (tok TIf *> parseExpr)

-- | Parse a local declaration in a where-clause.
-- NOTE: for now locals are unconditional
parseLocal :: Parser Local
parseLocal = Local <$> parseLHS <* tok TAssign <*> parseExpr
  where
    parseLHS = fromList <$> (parens (sepBy1 idTok $ tok TComma) <|> pure <$> idTok)   -- LHS is either a single id or a parenthesized tuple of ids

-- | Parse an expression using the operator precedence table.
parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm exprTable

-- | Operator precedence and associativity table for expression parsing.
exprTable :: [[Operator Parser Expr]]
exprTable = [
    [
      unary Sqrt,
      unary Floor
    ],
    [ 
      binaryR Pow
    ], 
    [
      unary' TMinus Neg,
      unary Not
    ],
    [ 
      binary Mult, 
      binary Div, 
      binary Mod 
    ], 
    [ 
      binary Add, 
      binary' TMinus Sub 
    ], 
    [ 
      binary Eq, 
      binary Neq, 
      binary LessEq, 
      binary GreaterEq, 
      binary Less, 
      binary Greater, 
      binary Divides
    ],
    [
      binary And
    ],
    [
      binary Or
    ]
  ]
  where
    unary op = Prefix (foldr1 (.) <$> some (Unary op <$ (tok $ TUOp op)))     -- chaining unary operators: https://www.stackage.org/haddock/lts-16.27/parser-combinators-1.2.1/Control-Monad-Combinators-Expr.html#v:makeExprParser
    unary' t op = Prefix (foldr1 (.) <$> some (Unary op <$ tok t))
    binary op = InfixL (Binary op <$ (tok $ TBOp op))
    binary' t op = InfixL (Binary op <$ tok t)
    binaryR op = InfixR (Binary op <$ (tok $ TBOp op))  -- right-associative

-- | Parse the atomic term forms in an expression.
parseTerm :: Parser Expr
parseTerm = parens (try parseTuple <|> parseExpr)
  <|> parseCall
  <|> ImmediateReal <$> realTok
  <|> ImmediateInt  <$> intTok
  <|> ImmediateBool <$> boolTok

-- | Parse a tuple expression with at least two elements.
parseTuple :: Parser Expr
parseTuple = Tuple <$> ((:|) <$> (parseExpr <* tok TComma) <*> (sepBy1 parseExpr $ tok TComma))

-- | Parse a function call expression.
parseCall :: Parser Expr
parseCall = Call <$> idTok <*> (option [] $ parens $ sepBy1 parseExpr $ tok TComma)

-- | Parse a block annotation inside `# [...]`.
parseBlockAnnotation :: Parser BlockAnnotation
parseBlockAnnotation = try (BlockDisplay <$> parseBlockDisplay)
                      <|> BlockClass <$> parseKeyValue "class"
                      <|> BlockName <$> parseKeyValue "name"
                      <|> BlockLabel <$> parseKeyValue "label"
                      <|> BlockDescription <$> parseKeyValue "description"
  where 
    parseKeyValue key = (tok $ TId key) *> (tok $ TBOp Eq) *> strTok

-- | Parse a block display mode annotation.
parseBlockDisplay :: Parser BlockDisplayMode
parseBlockDisplay = BoxBlock <$ keyTok "box"
                    <|> try (InTextBlock <$ keyTok "intext")
                    <|> try (InLineBlock <$ keyTok "inline")
                    <|> HiddenBlock <$ keyTok "hidden"

-- | Parse a declaration annotation inside `@[...]`.
parseDeclAnnotation :: Parser DeclAnnotation
parseDeclAnnotation = try (DeclDisplay <$> parseDeclDisplay)

-- | Parse a declaration display mode annotation.
parseDeclDisplay :: Parser DeclDisplayMode
parseDeclDisplay = HiddenDecl <$ keyTok "hidden"


-- Helper functions

-- | Consume a specific token, failing otherwise.
tok :: Token -> Parser ()
tok target = token test (Set.singleton (Tokens (target :| [])))
  where
    test t | t == target = Just ()
           | otherwise = Nothing

-- | Parse an identifier token and return its name.
idTok :: Parser String
idTok = tokExtract test
  where
    test (TId name) = Just name
    test _ = Nothing

-- | Parse a string literal token and return its value.
strTok :: Parser String
strTok = tokExtract test
  where
    test (TStr text) = Just text
    test _ = Nothing

-- | Parse a specific identifier keyword token.
keyTok :: String -> Parser ()
keyTok key = tok (TId key)

-- | Parse an integer token and return its numeric value.
intTok :: Parser Integer
intTok = tokExtract test
  where
    test (TInt i) = Just i
    test _ = Nothing

-- | Parse a real-number token and return its numeric value.
realTok :: Parser Double
realTok = tokExtract test
  where
    test (TReal r) = Just r
    test _ = Nothing

-- | Parse a boolean token and return its value.
boolTok :: Parser Bool
boolTok = tokExtract test
  where
    test (TBool b) = Just b
    test _ = Nothing

-- | Parse a primitive type token and return the type.
primTypeTok :: Parser PrimitiveType
primTypeTok = tokExtract test
  where
    test (TPrimType pt) = Just pt
    test _ = Nothing

-- | Extract a value from a token using a partial matcher.
tokExtract :: (Token -> Maybe a) -> Parser a
tokExtract = (`token` Set.empty)


-- | Parse a parenthesized parser result.
parens :: Parser a -> Parser a
parens = between (tok TLParen) (tok TRParen)

-- | Parse a bracketed parser result.
brackets :: Parser a -> Parser a
brackets = between (tok TLBracket) (tok TRBracket)

-- | Parse a braced parser result.
braces :: Parser a -> Parser a
braces = between (tok TLBrace) (tok TRBrace)