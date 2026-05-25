module CodeGenHaskell (codeGenHaskell) where
import Elab (ElabResult)
import Types (Fragment(..))
import IR
import Data.List (intercalate)
import AST (Signature (..), Type (..), PrimitiveType (..), Id)
import Data.List.NonEmpty (toList, NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

codeGenHaskell :: ElabResult -> String
codeGenHaskell frags = concat (map codeGenFragment frags)
    where   codeGenFragment (TextFragment _) = ""
            codeGenFragment (CodeFragment (IR decls)) = intercalate "\n\n" (map codeGenDeclaration decls)

codeGenDeclaration :: IRDeclaration -> String
codeGenDeclaration (IRDeclaration ident sig params impl locals constraints) = 
    ident ++ " :: " ++ codeGenSignature sig ++ "\n" ++
    ident ++ " " ++ intercalate " " params ++ "\n" ++
    concat (map (codeGenConstraint ident) constraints) ++
    codeGenImpl impl ++
    codeGenLocals locals

codeGenConstraint :: Id -> IRExpr -> String
codeGenConstraint ident e = "\t| not $ " ++ codeGenExpr e ++ " = error $ \"[" ++ ident ++ "] Violated constraint `" ++ show e ++ "`\"\n" -- TODO: use a pretty printed expression here instead, so it looks like in the DSL

codeGenImpl :: IRImplementation -> String
codeGenImpl (IRUnconditional e) = codeGenOther e
codeGenImpl (IRConditional branches other) = concat (map codeGenBranch branches) ++ codeGenOther other

codeGenBranch :: IRBranch -> String
codeGenBranch (IRBranch cond e) = "\t| " ++ codeGenExpr cond ++ " = " ++ codeGenExpr e ++ "\n"

codeGenOther :: IRExpr -> String
codeGenOther e = "\t| otherwise = " ++ codeGenExpr e ++ "\n"

codeGenLocals :: [IRLocal] -> String
codeGenLocals [] = ""
codeGenLocals locals =
    "\twhere\n" ++
    "\t\t" ++ (intercalate "\n\t\t" (map codeGenAssign locals))
    where   codeGenAssign (IRLocal idents e) = codeGenMaybeTuple idents ++ " = " ++ codeGenExpr e

codeGenExpr :: IRExpr -> String
codeGenExpr _ = "gnoerks"   -- TODO: implement this

codeGenSignature :: Signature -> String
codeGenSignature (Signature maybeFrom (Type to)) = fromPart ++ toPart
    where   fromPart = case maybeFrom of
                Nothing -> ""
                Just (Type from) -> intercalate " -> " (map codeGenPrimitiveType (toList from)) ++ " -> "
            toPart = codeGenMaybeTuple (NonEmpty.map codeGenPrimitiveType to)
            -- toPart  | length toPart == 1 = codeGenPrimitiveType pt1 
            --         | otherwise = "(" ++ intercalate ", " (map codeGenPrimitiveType (toList to)) ++ ")"
            
codeGenMaybeTuple :: NonEmpty String -> String
codeGenMaybeTuple (el :| []) = el
codeGenMaybeTuple els = "(" ++ intercalate ", " (toList els) ++ ")"

codeGenPrimitiveType :: PrimitiveType -> String
codeGenPrimitiveType Positive = "Positive"
codeGenPrimitiveType Natural = "Natural"
codeGenPrimitiveType Integer = "Integer"
codeGenPrimitiveType Rational = "Rational"
codeGenPrimitiveType Real = "Double"
codeGenPrimitiveType Boolean = "Bool"