{-# LANGUAGE TemplateHaskell #-}

module CodeGenHaskell (codeGenHaskell) where

import Elab (ElabResult)
import Types (Fragment(..))
import IR
import Data.List (intercalate)
import AST (Signature (..), Type (..), PrimitiveType (..), Id, UnaryOp (..))
import Data.List.NonEmpty (toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.FileEmbed (embedStringFile)
import CodeGen


extensions :: String
extensions = $(embedStringFile "prelude/extensions.hs")

moduleName :: String
moduleName = "MHL"

imports :: String
imports = $(embedStringFile "prelude/imports.hs")

prelude :: String
prelude = "-- PRELUDE --\n\n" ++ intercalate "\n\n" [positive, casting]
    where   positive = $(embedStringFile "prelude/positive.hs")
            casting = $(embedStringFile "prelude/casting.hs")

codeGenHaskell :: ElabResult -> String
codeGenHaskell frags = intercalate "\n\n" [extensions, moduleStr, imports, code, prelude]
    where   moduleStr = "module " ++ moduleName ++ " " ++ exportStr ++ " where"

            exportStr = case exports of
                [] -> "()"
                es -> parenTuple es

            exports = concat [[moduleName ++ "." ++ ident | (IRDeclaration ident _ _ _ _) <- decls] | (CodeFragment (IR decls)) <- frags]
        
            code = concat (map codeGenFragment frags)

            codeGenFragment (TextFragment _) = ""
            codeGenFragment (CodeFragment (IR decls)) = (intercalate "\n\n" (map codeGenDeclaration decls)) ++ "\n\n"

codeGenDeclaration :: IRDeclaration -> String
codeGenDeclaration (IRDeclaration ident sig params impl whereTerms) = 
    ident ++ " :: " ++ codeGenSignature sig ++ "\n" ++
    ident ++ " " ++ unwords params ++ "\n" ++
    concat (map (codeGenConstraint ident) constraints) ++
    codeGenImpl impl ++
    codeGenLocals locals
    where   constraints = [e | (IRConstraint e) <- whereTerms]
            locals      = [l | (IRLocalDecl l) <- whereTerms]

codeGenConstraint :: Id -> IRExpr -> String
codeGenConstraint ident e = tab ++ "| not " ++ codeGenExpr e ++ " = error \"[" ++ ident ++ "] Violated constraint `" ++ show e ++ "`\"\n" -- TODO: use a pretty printed expression here instead, so it looks like in the DSL

codeGenImpl :: IRImplementation -> String
codeGenImpl (IRUnconditional e) = codeGenOther e
codeGenImpl (IRConditional branches other) = concat (map codeGenBranch branches) ++ codeGenOther other

codeGenBranch :: IRBranch -> String
codeGenBranch (IRBranch e cond) = tab ++ "| " ++ unparens (codeGenExpr cond) ++ " = " ++ unparens (codeGenExpr e) ++ "\n"

codeGenOther :: IRExpr -> String
codeGenOther e = tab ++ "| otherwise = " ++ unparens (codeGenExpr e)

codeGenLocals :: [IRLocal] -> String
codeGenLocals [] = ""
codeGenLocals locals = "\n" ++
    tab ++ "where\n" ++
    tab ++ tab ++ (intercalate ("\n" ++ tab ++ tab) (map codeGenAssign locals))
    where   codeGenAssign (IRLocal idents e) = maybeParenTuple idents ++ " = " ++ unparens (codeGenExpr e)

codeGenSignature :: Signature -> String
codeGenSignature (Signature maybeFrom (Type tos)) = fromPart ++ toPart
    where   fromPart = case maybeFrom of
                Nothing -> ""
                Just (Type from) -> intercalate arrow (map codeGenPrimitiveType (toList from)) ++ arrow
            toPart = maybeParenTuple (NonEmpty.map codeGenPrimitiveType tos)
            
codeGenExpr :: IRExpr -> String
codeGenExpr (IRCast e from to) = parens $ codeGenCast from to ++ " " ++ codeGenExpr e   -- TODO: consider implementing special cases for simplifying literal casting
codeGenExpr (IRCall ident isGlobal []) = maybeGlobalIdent ident isGlobal
codeGenExpr (IRCall ident isGlobal es) = parens $ (maybeGlobalIdent ident isGlobal) ++ " " ++ unwords (map codeGenExpr es) 
codeGenExpr (IRImmediateInt i pt) = parens $ show i ++ " :: " ++ codeGenPrimitiveType pt
codeGenExpr (IRImmediateReal r) = parens $ show r ++ " :: Double"
codeGenExpr (IRImmediateBool b) = show b
codeGenExpr (IRBinary op e1 e2) = parens $ codeGenBinary op e1 e2
codeGenExpr (IRUnary op e) = parens $ codeGenUnary op e
codeGenExpr (IRTuple es) = parenTuple $ map (unparens . codeGenExpr) (toList es)

maybeGlobalIdent :: Id -> Bool -> Id
maybeGlobalIdent ident False = ident
maybeGlobalIdent ident True = moduleName ++ "." ++ ident

codeGenCast :: PrimitiveType -> PrimitiveType -> String
codeGenCast from to 
    | from == Boolean || to == Boolean = error $ "LOGIC ERROR: codeGenCast called with Boolean type(s)"
    | from == to = error $ "LOGIC ERROR: codeGenCast called with two of the same types"
    | from < to = "widen @" ++ codeGenPrimitiveType to
    | otherwise = "narrow @" ++ codeGenPrimitiveType to

codeGenUnary :: UnaryOp -> IRExpr -> String
codeGenUnary Floor e = "floor " ++ codeGenExpr e
codeGenUnary Sqrt e = "sqrt " ++ codeGenExpr e

codeGenBinary :: IRBinaryOp -> IRExpr -> IRExpr -> String
codeGenBinary IRAdd = infixOp "+"
codeGenBinary IRSub = infixOp "-"
codeGenBinary IRMult = infixOp "*"
codeGenBinary IRFrac = infixOp "%"
codeGenBinary IRDiv = infixOp "/"
codeGenBinary IRPow = infixOp "^"
codeGenBinary IRExp = infixOp "**"
codeGenBinary IRMod = infixOp "`mod`"
codeGenBinary IREq = infixOp "=="
codeGenBinary IRNeq = infixOp "/="
codeGenBinary IRLess = infixOp "<"
codeGenBinary IRGreater = infixOp ">"
codeGenBinary IRLessEq = infixOp "<="
codeGenBinary IRGreaterEq = infixOp ">="
codeGenBinary IRDivides = (\e1 e2 -> codeGenExpr e2 ++ symbol "`mod`" ++ codeGenExpr e1 ++ " == 0")

codeGenPrimitiveType :: PrimitiveType -> String
codeGenPrimitiveType Positive = "Positive"
codeGenPrimitiveType Natural = "Natural"
codeGenPrimitiveType Integer = "Integer"
codeGenPrimitiveType Rational = "Rational"
codeGenPrimitiveType Real = "Double"
codeGenPrimitiveType Boolean = "Bool"

infixOp = infixBinaryOp codeGenExpr
maybeParenTuple = maybeTuple "(" ")"
parenTuple = tuple "(" ")"
parens = wrap "(" ")"
unparens = unwrap "(" ")"

arrow = symbol "->"