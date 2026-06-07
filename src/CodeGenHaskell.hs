{-# LANGUAGE TemplateHaskell #-}

module CodeGenHaskell (codeGenHaskell, codeGenExpr) where

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

imports :: String
imports = $(embedStringFile "prelude/imports.hs")

prelude :: String
prelude = "-- PRELUDE --\n\n" ++ intercalate "\n\n" [positive, casting]
    where   positive = $(embedStringFile "prelude/positive.hs")
            casting = $(embedStringFile "prelude/casting.hs")

-- returns a tuple of the Haskell library file string and a list of eval fragment Haskell expression strings
codeGenHaskell :: ElabResult -> String -> (String, [String])
codeGenHaskell frags moduleName = (intercalate "\n\n" [extensions, moduleStr, imports, libCode, prelude], evalCode)
    where   moduleStr = "module " ++ moduleName ++ " " ++ exportStr ++ " where"

            exportStr = case exports of
                [] -> "()"
                es -> parenTuple es

            exports = concat [[moduleName ++ "." ++ ident | (IRDeclaration _ ident _ _ _ _) <- decls] | (DefinitionFragment (IR _ decls)) <- frags]
        
            libCode = concat (map codeGenFragment frags)

            -- codeGenFragment (TextFragment _) = ""
            codeGenFragment (DefinitionFragment (IR _ decls)) = (intercalate "\n\n" (map (`codeGenDeclaration` moduleName) decls)) ++ "\n\n"
            codeGenFragment _ = ""

            evalCode = [gen e | EvalFragment e <- frags, let gen = (`codeGenExpr` moduleName)]
            -- codeGenFragment (EvalFragment e) = codeGenExpr e moduleName

codeGenDeclaration :: IRDeclaration -> String -> String
codeGenDeclaration (IRDeclaration _ ident sig params impl whereTerms) moduleName = 
    ident ++ " :: " ++ codeGenSignature sig ++ "\n" ++
    ident ++ " " ++ unwords params ++ "\n" ++
    concat (map (\e -> codeGenConstraint ident e moduleName) constraints) ++
    codeGenImpl impl moduleName ++
    codeGenLocals locals moduleName
    where   constraints = [e | (IRConstraint e) <- whereTerms]
            locals      = [l | (IRLocalDecl l) <- whereTerms]

codeGenConstraint :: Id -> IRExpr -> String -> String
codeGenConstraint ident e moduleName = tab ++ "| not " ++ codeGenExpr e moduleName ++ " = error \"[" ++ ident ++ "] Violated constraint `" ++ show e ++ "`\"\n" -- TODO: use a pretty printed expression here instead, so it looks like in the DSL

codeGenImpl :: IRImplementation -> String -> String
codeGenImpl (IRUnconditional e) moduleName = codeGenOther e moduleName
codeGenImpl (IRConditional branches other) moduleName = concat (map (`codeGenBranch` moduleName) branches) ++ codeGenOther other moduleName

codeGenBranch :: IRBranch -> String -> String
codeGenBranch (IRBranch e cond) moduleName = tab ++ "| " ++ unparens (codeGenExpr cond moduleName) ++ " = " ++ unparens (codeGenExpr e moduleName) ++ "\n"

codeGenOther :: IRExpr -> String -> String
codeGenOther e moduleName = tab ++ "| otherwise = " ++ unparens (codeGenExpr e moduleName)

codeGenLocals :: [IRLocal] -> String -> String
codeGenLocals [] _ = ""
codeGenLocals locals moduleName = "\n" ++
    tab ++ "where\n" ++
    tab ++ tab ++ (intercalate ("\n" ++ tab ++ tab) (map codeGenAssign locals))
    where   codeGenAssign (IRLocal idents e) = maybeParenTuple idents ++ " = " ++ unparens (codeGenExpr e moduleName)

codeGenSignature :: Signature -> String
codeGenSignature (Signature maybeFrom (Type tos)) = fromPart ++ toPart
    where   fromPart = case maybeFrom of
                Nothing -> ""
                Just (Type from) -> intercalate arrow (map codeGenPrimitiveType (toList from)) ++ arrow
            toPart = maybeParenTuple (NonEmpty.map codeGenPrimitiveType tos)
            
codeGenExpr :: IRExpr -> String -> String
codeGenExpr (IRCast e from to) moduleName = parens $ codeGenCast from to ++ " " ++ codeGenExpr e moduleName   -- TODO: consider implementing special cases for simplifying literal casting
codeGenExpr (IRCall ident isGlobal []) moduleName = maybeGlobalIdent ident isGlobal moduleName
codeGenExpr (IRCall ident isGlobal es) moduleName = parens $ (maybeGlobalIdent ident isGlobal moduleName) ++ " " ++ unwords (map (`codeGenExpr` moduleName) es) 
codeGenExpr (IRImmediateInt i pt) _ = parens $ show i ++ " :: " ++ codeGenPrimitiveType pt
codeGenExpr (IRImmediateReal r) _ = parens $ show r ++ " :: Double"
codeGenExpr (IRImmediateBool b) _ = show b
codeGenExpr (IRBinary op e1 e2) moduleName = parens $ codeGenBinary op e1 e2 moduleName
codeGenExpr (IRUnary op e) moduleName = parens $ codeGenUnary op e moduleName
codeGenExpr (IRTuple es) moduleName = parenTuple $ map (unparens . (`codeGenExpr` moduleName)) (toList es)

maybeGlobalIdent :: Id -> Bool -> String -> Id
maybeGlobalIdent ident False _ = ident
maybeGlobalIdent ident True moduleName = moduleName ++ "." ++ ident

codeGenCast :: PrimitiveType -> PrimitiveType -> String
codeGenCast from to 
    | from == Boolean || to == Boolean = error $ "LOGIC ERROR: codeGenCast called with Boolean type(s)"
    | from == to = error $ "LOGIC ERROR: codeGenCast called with two of the same types"
    | from < to = "widen @" ++ codeGenPrimitiveType to
    | otherwise = "narrow @" ++ codeGenPrimitiveType to

codeGenUnary :: UnaryOp -> IRExpr -> String -> String
codeGenUnary Neg e moduleName = "-" ++ codeGenExpr e moduleName
codeGenUnary Floor e moduleName = "floor " ++ codeGenExpr e moduleName
codeGenUnary Sqrt e moduleName = "sqrt " ++ codeGenExpr e moduleName

codeGenBinary :: IRBinaryOp -> IRExpr -> IRExpr -> String -> String
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
codeGenBinary IRDivides = (\e1 e2 moduleName -> codeGenExpr e2 moduleName ++ symbol "`mod`" ++ codeGenExpr e1 moduleName ++ " == 0")

codeGenPrimitiveType :: PrimitiveType -> String
codeGenPrimitiveType Positive = "Positive"
codeGenPrimitiveType Natural = "Natural"
codeGenPrimitiveType Integer = "Integer"
codeGenPrimitiveType Rational = "Rational"
codeGenPrimitiveType Real = "Double"
codeGenPrimitiveType Boolean = "Bool"

infixOp op e1 e2 moduleName = infixBinaryOp (`codeGenExpr` moduleName) op e1 e2
maybeParenTuple = maybeTuple "(" ")"
parenTuple = tuple "(" ")"
parens = wrap "(" ")"
unparens = unwrap "(" ")"

arrow = symbol "->"