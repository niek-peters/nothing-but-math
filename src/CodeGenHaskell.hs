{-# LANGUAGE TemplateHaskell #-}

-- | Haskell code generation. Takes elaboration phase output and produces Haskell code.
module CodeGenHaskell (codeGenHaskell, codeGenExpr) where

import Elab (ElabResult)
import Types (Fragment(..))
import IR
import Data.List (intercalate)
import AST (Signature (..), Type (..), Id)
import Data.List.NonEmpty (toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.FileEmbed (embedStringFile)
import CodeGen
import Token (UnaryOp (..), PrimitiveType (..))


-- | Extensions to enable in the generated Haskell module.
extensions :: String
extensions = $(embedStringFile "prelude/extensions.hs")

-- | Imports required by the generated Haskell module.
imports :: String
imports = $(embedStringFile "prelude/imports.hs")

-- | Prelude code injected into the generated module (Positive type and casting functions).
prelude :: String
prelude = "-- PRELUDE --\n\n" ++ intercalate "\n\n" [positive, casting]
    where   positive = $(embedStringFile "prelude/positive.hs")
            casting = $(embedStringFile "prelude/casting.hs")

-- | Generate a Haskell module and a list of evaluation expressions from elaboration fragments.
codeGenHaskell :: ElabResult -> String -> (String, [String])
codeGenHaskell frags moduleName = (intercalate "\n\n" [extensions, moduleStr, imports, libCode, prelude], evalCode)
    where   moduleStr = "module " ++ moduleName ++ " " ++ exportStr ++ " where"

            exportStr = case exports of
                [] -> "()"
                es -> parenTuple es

            exports = concat [[moduleName ++ "." ++ ident | (IRDeclaration _ ident _ _ _ _) <- decls] | (DefinitionFragment (IR _ decls)) <- frags]
        
            libCode = concat (map codeGenFragment frags)

            codeGenFragment (DefinitionFragment (IR _ decls)) = (intercalate "\n\n" (map (`codeGenDeclaration` moduleName) decls)) ++ "\n\n"
            codeGenFragment _ = ""

            evalCode = [gen e | EvalFragment e <- frags, let gen = (`codeGenExpr` moduleName)]

-- | Emit a Haskell declaration for an IR declaration in the given module namespace.
codeGenDeclaration :: IRDeclaration -> String -> String
codeGenDeclaration (IRDeclaration _ ident sig params impl whereTerms) moduleName = 
    ident ++ " :: " ++ codeGenSignature sig ++ "\n" ++
    ident ++ " " ++ unwords params ++ "\n" ++
    concat (map (\e -> codeGenConstraint ident e moduleName) constraints) ++
    codeGenImpl impl moduleName ++
    codeGenLocals locals moduleName
    where   constraints = [e | (IRConstraint e) <- whereTerms]
            locals      = [l | (IRLocalDecl l) <- whereTerms]

-- | Generate a runtime constraint check for a declaration; throws an error on violation.
codeGenConstraint :: Id -> IRExpr -> String -> String
codeGenConstraint ident e moduleName = tab ++ "| not " ++ codeGenExpr e moduleName ++ " = error \"[" ++ ident ++ "] Violated constraint `" ++ show e ++ "`\"\n" -- Future Work: use a pretty printed expression here instead, so it looks like in the DSL

-- | Render the implementation body: either unconditional or conditional branches.
codeGenImpl :: IRImplementation -> String -> String
codeGenImpl (IRUnconditional e) moduleName = codeGenOther e moduleName
codeGenImpl (IRConditional branches other) moduleName = concat (map (`codeGenBranch` moduleName) branches) ++ codeGenOther other moduleName

-- | Emit a single guarded branch for a conditional implementation.
codeGenBranch :: IRBranch -> String -> String
codeGenBranch (IRBranch e cond) moduleName = tab ++ "| " ++ unparens (codeGenExpr cond moduleName) ++ " = " ++ unparens (codeGenExpr e moduleName) ++ "\n"

-- | Emit the fallback `otherwise` branch for conditional implementations.
codeGenOther :: IRExpr -> String -> String
codeGenOther e moduleName = tab ++ "| otherwise = " ++ unparens (codeGenExpr e moduleName)

-- | Emit `where` local declarations.
codeGenLocals :: [IRLocal] -> String -> String
codeGenLocals [] _ = ""
codeGenLocals locals moduleName = "\n" ++
    tab ++ "where\n" ++
    tab ++ tab ++ (intercalate ("\n" ++ tab ++ tab) (map codeGenAssign locals))
    where   codeGenAssign (IRLocal idents e) = maybeParenTuple idents ++ " = " ++ unparens (codeGenExpr e moduleName)

-- | Convert an AST `Signature` into a Haskell type signature string.
codeGenSignature :: Signature -> String
codeGenSignature (Signature maybeFrom (Type tos)) = fromPart ++ toPart
    where   fromPart = case maybeFrom of
                Nothing -> ""
                Just (Type from) -> intercalate arrow (map codeGenPrimitiveType (toList from)) ++ arrow
            toPart = maybeParenTuple (NonEmpty.map codeGenPrimitiveType tos)
            
-- | Render an `IRExpr` as a Haskell expression string qualified into the given module.
codeGenExpr :: IRExpr -> String -> String
codeGenExpr (IRCast e from to) moduleName = parens $ codeGenCast from to ++ " " ++ codeGenExpr e moduleName   -- Future Work: consider implementing special cases for simplifying literal casting
codeGenExpr (IRCall ident isGlobal []) moduleName = maybeGlobalIdent ident isGlobal moduleName
codeGenExpr (IRCall ident isGlobal es) moduleName = parens $ (maybeGlobalIdent ident isGlobal moduleName) ++ " " ++ unwords (map (`codeGenExpr` moduleName) es) 
codeGenExpr (IRImmediateInt i pt) _ = parens $ show i ++ " :: " ++ codeGenPrimitiveType pt
codeGenExpr (IRImmediateReal r) _ = parens $ show r ++ " :: Double"
codeGenExpr (IRImmediateBool b) _ = show b
codeGenExpr (IRBinary op e1 e2) moduleName = parens $ codeGenBinary op e1 e2 moduleName
codeGenExpr (IRUnary op e) moduleName = parens $ codeGenUnary op e moduleName
codeGenExpr (IRTuple es) moduleName = parenTuple $ map (unparens . (`codeGenExpr` moduleName)) (toList es)

-- | Qualify an identifier with the module name when `isGlobal` is True.
maybeGlobalIdent :: Id -> Bool -> String -> Id
maybeGlobalIdent ident False _ = ident
maybeGlobalIdent ident True moduleName = moduleName ++ "." ++ ident

-- | Generate a runtime cast (widen/narrow) between primitive types; panics on Boolean involvement.
codeGenCast :: PrimitiveType -> PrimitiveType -> String
codeGenCast from to 
    | from == Boolean || to == Boolean = error $ "LOGIC ERROR: codeGenCast called with Boolean type(s)"
    | from == to = error $ "LOGIC ERROR: codeGenCast called with two of the same types"
    | from < to = "widen @" ++ codeGenPrimitiveType to
    | otherwise = "narrow @" ++ codeGenPrimitiveType to

-- | Generate Haskell code for unary IR operations.
codeGenUnary :: UnaryOp -> IRExpr -> String -> String
codeGenUnary Neg e moduleName = "-" ++ codeGenExpr e moduleName
codeGenUnary Floor e moduleName = (parens $ "floor " ++ codeGenExpr e moduleName) ++ " :: Integer"  -- return type of floor can cause ambiguity. We explicitly tell Haskell it's an Integer here
codeGenUnary Sqrt e moduleName = "sqrt " ++ codeGenExpr e moduleName
codeGenUnary Not e moduleName = "not " ++ codeGenExpr e moduleName

-- | Generate Haskell code for binary IR operations.
codeGenBinary :: IRBinaryOp -> IRExpr -> IRExpr -> String -> String
codeGenBinary IRAdd = infixOp "+"
codeGenBinary IRSub = infixOp "-"
codeGenBinary IRMult = infixOp "*"
codeGenBinary IRFrac = infixOp "%"
codeGenBinary IRDiv = infixOp "/"
codeGenBinary IRPosPow = infixOp "^"
codeGenBinary IRFracPow = infixOp "^^"
codeGenBinary IRFloatPow = infixOp "**"
codeGenBinary IRMod = infixOp "`mod`"
codeGenBinary IREq = infixOp "=="
codeGenBinary IRNeq = infixOp "/="
codeGenBinary IRLess = infixOp "<"
codeGenBinary IRGreater = infixOp ">"
codeGenBinary IRLessEq = infixOp "<="
codeGenBinary IRGreaterEq = infixOp ">="
codeGenBinary IRDivides = (\e1 e2 moduleName -> codeGenExpr e2 moduleName ++ symbol "`mod`" ++ codeGenExpr e1 moduleName ++ " == 0")
codeGenBinary IRAnd = infixOp "&&"
codeGenBinary IROr = infixOp "||"

-- | Helper to render a binary infix operator between two expression strings.
infixOp :: String -> IRExpr -> IRExpr -> String -> String
infixOp op e1 e2 moduleName = (codeGenExpr e1 moduleName) ++ symbol op ++ (codeGenExpr e2 moduleName)

-- | Map internal `PrimitiveType` to its Haskell type name as a string.
codeGenPrimitiveType :: PrimitiveType -> String
codeGenPrimitiveType Positive = "Positive"
codeGenPrimitiveType Natural = "Natural"
codeGenPrimitiveType Integer = "Integer"
codeGenPrimitiveType Rational = "Rational"
codeGenPrimitiveType Real = "Double"
codeGenPrimitiveType Boolean = "Bool"

-- Convenience wrappers for tuple/paren rendering used throughout code generation.
maybeParenTuple :: NonEmpty.NonEmpty String -> String
maybeParenTuple = maybeTuple "(" ")"

parenTuple :: [String] -> String
parenTuple = tuple "(" ")"

parens :: String -> String
parens = wrap "(" ")"

unparens :: String -> String
unparens = unwrap "(" ")"

-- | Arrow string used when assembling function types.
arrow :: String
arrow = symbol "->"