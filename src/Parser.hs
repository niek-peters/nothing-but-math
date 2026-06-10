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

type Parser = Parsec Void [Token]

type ParseResult = [Fragment String AST Expr]

parse :: TokenizeResult -> ParseResult
parse = map runFragmentParser

useParser :: Parser a -> [Token] -> a
useParser parser ts = case Text.Megaparsec.parse parser "" ts of
    Left e -> error $ errorBundlePretty e
    Right r -> r

runExprParser :: [Token] -> Expr
runExprParser = useParser parseExpr

runFragmentParser :: Fragment String [Token] [Token] -> Fragment String AST Expr
runFragmentParser (TextFragment t) = TextFragment t
runFragmentParser (DefinitionFragment c) = (DefinitionFragment $ useParser (parseCodeFragment <* eof) c)
runFragmentParser (EvalFragment e) = (EvalFragment $ useParser (parseExpr <* eof) e)

parseCodeFragment :: Parser AST
parseCodeFragment = AST <$> option [] (tok THash *> brackets (sepBy parseBlockAnnotation $ tok TComma)) <*> many parseDeclaration

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

parseSignature :: Parser Signature
parseSignature = toSignature <$> parseType <*> optional (tok TArrow *> parseType)
    where
        toSignature t1 Nothing   = Signature Nothing t1     -- there is no '->'. This is a constant
        toSignature t1 (Just t2) = Signature (Just t1) t2   

parseType :: Parser Type
parseType = Type . fromList <$> sepBy1 primTypeTok (tok $ TId "x")  -- the 'cross'/x symbol is tokenized as an identifier x

parseImplementation :: Parser Implementation
parseImplementation = braces parseConditional <|> Unconditional <$> parseExpr
    where
        parseConditional = Conditional <$> many (try parseBranch) <*> (parseExpr <* tok TOtherwise)     -- TODO: consider not using many but something that requires at least 1
        parseBranch = Branch <$> parseExpr <*> (tok TIf *> parseExpr)

-- NOTE: for now locals are unconditional
parseLocal :: Parser Local
parseLocal = Local <$> parseLHS <* tok TAssign <*> parseExpr
  where
    parseLHS = fromList <$> (parens (sepBy1 idTok $ tok TComma) <|> pure <$> idTok)   -- LHS is either a single id or a parenthesized tuple of ids

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm exprTable

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

parseTerm :: Parser Expr
parseTerm = parens (try parseTuple <|> parseExpr)
  <|> parseCall
  <|> ImmediateReal <$> realTok
  <|> ImmediateInt  <$> intTok
  <|> ImmediateBool <$> boolTok

-- NOTE: we explicitly disallow single-element tuples here
parseTuple :: Parser Expr
parseTuple = Tuple <$> ((:|) <$> (parseExpr <* tok TComma) <*> (sepBy1 parseExpr $ tok TComma))

parseCall :: Parser Expr
parseCall = Call <$> idTok <*> (option [] $ parens $ sepBy1 parseExpr $ tok TComma)

parseBlockAnnotation :: Parser BlockAnnotation
parseBlockAnnotation = try (BlockDisplay <$> parseBlockDisplay)
                      <|> BlockClass <$> parseKeyValue "class"
                      <|> BlockName <$> parseKeyValue "name"
                      <|> BlockLabel <$> parseKeyValue "label"
                      <|> BlockDescription <$> parseKeyValue "description"
  where 
    parseKeyValue key = (tok $ TId key) *> (tok $ TBOp Eq) *> strTok

parseBlockDisplay :: Parser BlockDisplayMode
parseBlockDisplay = DefaultBlock <$ keyTok "default"
                    <|> BoxBlock <$ keyTok "box"
                    <|> try (InTextBlock <$ keyTok "intext")
                    <|> try (InLineBlock <$ keyTok "inline")
                    <|> HiddenBlock <$ keyTok "hidden"

parseDeclAnnotation :: Parser DeclAnnotation
parseDeclAnnotation = try (DeclDisplay <$> parseDeclDisplay)

parseDeclDisplay :: Parser DeclDisplayMode
parseDeclDisplay = DefaultDecl <$ keyTok "default"
                    <|> HiddenDecl <$ keyTok "hidden"


-- Helper functions

tok :: Token -> Parser ()
tok target = token test (Set.singleton (Tokens (target :| [])))
  where
    test t | t == target = Just ()
           | otherwise = Nothing

idTok :: Parser String
idTok = tokExtract test
  where
    test (TId name) = Just name
    test _ = Nothing

strTok :: Parser String
strTok = tokExtract test
  where
    test (TStr text) = Just text
    test _ = Nothing

keyTok :: String -> Parser ()
keyTok key = tok (TId key)

intTok :: Parser Integer
intTok = tokExtract test
  where
    test (TInt i) = Just i
    test _ = Nothing

realTok :: Parser Double
realTok = tokExtract test
  where
    test (TReal r) = Just r
    test _ = Nothing

boolTok :: Parser Bool
boolTok = tokExtract test
  where
    test (TBool b) = Just b
    test _ = Nothing

primTypeTok :: Parser PrimitiveType
primTypeTok = tokExtract test
  where
    test (TPrimType pt) = Just pt
    test _ = Nothing

tokExtract :: (Token -> Maybe a) -> Parser a
tokExtract = (`token` Set.empty)


parens :: Parser a -> Parser a
parens = between (tok TLParen) (tok TRParen)

brackets :: Parser a -> Parser a
brackets = between (tok TLBracket) (tok TRBracket)

braces :: Parser a -> Parser a
braces = between (tok TLBrace) (tok TRBrace)