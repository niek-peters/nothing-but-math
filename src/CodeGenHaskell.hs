{-# LANGUAGE TemplateHaskell #-}

module CodeGenHaskell (codeGenHaskell) where

import Elab (ElabResult)
import Types (Fragment(..))
import IR
import Data.List (intercalate)
import AST (Signature (..), Type (..), PrimitiveType (..), Id, UnaryOp (..))
import Data.List.NonEmpty (toList, NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

-- prelude :: String
-- prelude = $(embedStringFile)

codeGenHaskell :: ElabResult -> String
codeGenHaskell frags = concat (map codeGenFragment frags)
    where   codeGenFragment (TextFragment _) = ""
            codeGenFragment (CodeFragment (IR decls)) = intercalate "\n\n" (map codeGenDeclaration decls)

codeGenDeclaration :: IRDeclaration -> String
codeGenDeclaration (IRDeclaration ident sig params impl locals constraints) = 
    ident ++ " :: " ++ codeGenSignature sig ++ "\n" ++
    ident ++ " " ++ unwords params ++ "\n" ++
    concat (map (codeGenConstraint ident) constraints) ++
    codeGenImpl impl ++
    codeGenLocals locals

codeGenConstraint :: Id -> IRExpr -> String
codeGenConstraint ident e = "\t| not " ++ codeGenExpr e ++ " = error \"[" ++ ident ++ "] Violated constraint `" ++ show e ++ "`\"\n" -- TODO: use a pretty printed expression here instead, so it looks like in the DSL

codeGenImpl :: IRImplementation -> String
codeGenImpl (IRUnconditional e) = codeGenOther e
codeGenImpl (IRConditional branches other) = concat (map codeGenBranch branches) ++ codeGenOther other

codeGenBranch :: IRBranch -> String
codeGenBranch (IRBranch e cond) = "\t| " ++ unparens (codeGenExpr cond) ++ " = " ++ unparens (codeGenExpr e) ++ "\n"

codeGenOther :: IRExpr -> String
codeGenOther e = "\t| otherwise = " ++ unparens (codeGenExpr e) ++ "\n"

codeGenLocals :: [IRLocal] -> String
codeGenLocals [] = ""
codeGenLocals locals =
    "\twhere\n" ++
    "\t\t" ++ (intercalate "\n\t\t" (map codeGenAssign locals))
    where   codeGenAssign (IRLocal idents e) = maybeTuple idents ++ " = " ++ unparens (codeGenExpr e)

codeGenSignature :: Signature -> String
codeGenSignature (Signature maybeFrom (Type to)) = fromPart ++ toPart
    where   fromPart = case maybeFrom of
                Nothing -> ""
                Just (Type from) -> intercalate " -> " (map codeGenPrimitiveType (toList from)) ++ " -> "
            toPart = maybeTuple (NonEmpty.map codeGenPrimitiveType to)
            

codeGenExpr :: IRExpr -> String
codeGenExpr (IRCast e from to) = parens $ codeGenCast from to ++ " " ++ codeGenExpr e
codeGenExpr (IRCall ident []) = ident
codeGenExpr (IRCall ident es) = parens $ ident ++ " " ++ unwords (map codeGenExpr es) 
codeGenExpr (IRImmediateInt i) = show i
codeGenExpr (IRImmediateReal r) = show r
codeGenExpr (IRImmediateBool b) = show b
codeGenExpr (IRBinary op e1 e2) = parens $ codeGenBinary op e1 e2
codeGenExpr (IRUnary op e) = parens $ codeGenUnary op e
codeGenExpr (IRTuple es) = tuple $ NonEmpty.map codeGenExpr es

codeGenCast :: PrimitiveType -> PrimitiveType -> String
codeGenCast Positive _ = "fromIntegral"
codeGenCast Natural _ = "fromIntegral"
codeGenCast Integer _ = "fromIntegral"
codeGenCast Real Rational = "toRational"
codeGenCast Rational Real = "fromRational"
codeGenCast f t = error $ "LOGIC ERROR: codeGenCast called with invalid types, namely from '" ++ show f ++ "' to '" ++ show t ++ "'"

codeGenUnary :: UnaryOp -> IRExpr -> String
codeGenUnary Floor e = "floor " ++ codeGenExpr e
codeGenUnary Sqrt e = "sqrt " ++ codeGenExpr e

codeGenBinary :: IRBinaryOp -> IRExpr -> IRExpr -> String
codeGenBinary IRAdd = infixOp "+"
codeGenBinary IRSub = infixOp "-"
codeGenBinary IRMult = infixOp "*"
codeGenBinary IRDiv = (\e1 e2 -> (parens $ codeGenExpr e1 ++ " / " ++ codeGenExpr e2) ++ " :: Rational")
codeGenBinary IRPow = infixOp "^"
codeGenBinary IRExp = infixOp "**"
codeGenBinary IRMod = infixOp "`mod`"
codeGenBinary IREq = infixOp "=="
codeGenBinary IRNeq = infixOp "/="
codeGenBinary IRLess = infixOp "<"
codeGenBinary IRGreater = infixOp ">"
codeGenBinary IRLessEq = infixOp "<="
codeGenBinary IRGreaterEq = infixOp ">="
codeGenBinary IRDivides = (\e1 e2 -> codeGenExpr e2 ++ " `mod` " ++ codeGenExpr e1 ++ " == 0")

infixOp :: String -> IRExpr -> IRExpr -> String
infixOp opStr e1 e2 = codeGenExpr e1 ++ " " ++ opStr ++ " " ++ codeGenExpr e2

-- codeGenParensExpr :: IRExpr -> String
-- codeGenParensExpr = codeGenParens . codeGenExpr

maybeTuple :: NonEmpty String -> String
maybeTuple (el :| []) = el
maybeTuple els = tuple els

tuple :: NonEmpty String -> String
tuple els = parens $ intercalate ", " (toList els)

parens :: String -> String
parens str = "(" ++ str ++ ")"

-- removes outer parentheses if present
-- intended for use around codeGenExpr
unparens :: String -> String
unparens str    | length str < 2 = str
                | ',' `elem` str = str  -- don't remove parens from tuples
                | hasOpenParen && hasCloseParen = (init . tail) str
                | otherwise = str
    where   hasOpenParen = head str == '('
            hasCloseParen = last str == ')'

codeGenPrimitiveType :: PrimitiveType -> String
codeGenPrimitiveType Positive = "Positive"
codeGenPrimitiveType Natural = "Natural"
codeGenPrimitiveType Integer = "Integer"
codeGenPrimitiveType Rational = "Rational"
codeGenPrimitiveType Real = "Double"
codeGenPrimitiveType Boolean = "Bool"