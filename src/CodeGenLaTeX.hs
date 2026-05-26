module CodeGenLaTeX (codeGenLaTeX) where

import Elab (ElabResult)
import Types (Fragment(..))
import IR (IR(..), IRDeclaration (IRDeclaration), IRImplementation (..), IRLocal (..), IRExpr, IRWhereTerm (IRLocalDecl, IRConstraint), IRBranch (..))
import Data.List (intercalate)
import Data.List.NonEmpty (toList, fromList, NonEmpty)
import AST (Signature (Signature), PrimitiveType (..), Type (..))
import CodeGen
import qualified Data.List.NonEmpty as NonEmpty

data BlockType = Text | Table

-- TODO: remove hardcoded Text and get it from DSL annotation 
hardCodedBlockType :: BlockType
hardCodedBlockType = Text

codeGenLaTeX :: ElabResult -> String
codeGenLaTeX frags = concat (map codeGenFragment frags)
    where   codeGenFragment (TextFragment str) = str
            codeGenFragment (CodeFragment (IR decls)) = wrapBlock hardCodedBlockType $ intercalate newline $ (map (`codeGenBlock` hardCodedBlockType) decls)    

            wrapBlock Text = flalign
            wrapBlock Table = makecell

-- here we handle different LaTeX block types
-- we wrap the result in a certain block type and pass a statement wrapper function
codeGenBlock :: IRDeclaration -> BlockType -> String
codeGenBlock decl bt = codeGenDeclaration decl (wrapStatement bt)

wrapStatement :: BlockType -> String -> String
wrapStatement Text str = "&" ++ str ++ "&"
wrapStatement Table str = "$" ++ str ++ "$"

codeGenDeclaration :: IRDeclaration -> (String -> String) -> String
codeGenDeclaration (IRDeclaration ident sig params impl whereTerms) wrap = 
    wrap (text "Define" ++ textSep ++ ident ++ symbol ":" ++ codeGenSignature sig ++ textSep ++ text "by") ++ newline ++
    wrap (ident ++ maybeTuple' params ++ symbol "=" ++ codeGenImpl impl ++ "." `insertIf` noWherePart) ++ newline ++
    (wrap (codeGenWhere (fromList whereTerms) ++ ".") ++ newline) `insertIf` not noWherePart
    where   noWherePart = null whereTerms

codeGenSignature :: Signature -> String
codeGenSignature (Signature maybeFromT (Type tos)) = fromPart ++ toPart
    where   fromPart = case maybeFromT of
                Nothing -> ""
                Just (Type froms) -> typeTuple froms ++ arrow
            toPart = typeTuple tos

            typeTuple pts = intercalate cross (map codeGenPrimitiveType (toList pts)) 

codeGenImpl :: IRImplementation -> String
codeGenImpl (IRUnconditional e) = codeGenExpr e
codeGenImpl (IRConditional branches other) = block "cases" $ intercalate newline (map codeGenBranch branches ++ [codeGenOther other])-- concat (map codeGenBranch branches) ++ codeGenOther other

codeGenBranch :: IRBranch -> String
codeGenBranch (IRBranch e cond) = tab ++ unparens (codeGenExpr cond) ++ ", & " ++ text "if" ++ textSep ++ unparens (codeGenExpr e)

codeGenOther :: IRExpr -> String
codeGenOther e = tab ++ unparens (codeGenExpr e) ++ ", & " ++ text "otherwise"

codeGenWhere :: NonEmpty IRWhereTerm -> String
codeGenWhere whereTerms = text "where" ++ textSep ++ intercalateSpecialLast ", " (textSep ++ text "and" ++ textSep) (NonEmpty.map codeGenWhereTerm whereTerms)

codeGenWhereTerm :: IRWhereTerm -> String
codeGenWhereTerm (IRLocalDecl (IRLocal idents e)) = maybeTuple idents ++ symbol "=" ++ codeGenExpr e
codeGenWhereTerm (IRConstraint e) = codeGenExpr e

-- TODO: implement this
codeGenExpr :: IRExpr -> String
codeGenExpr e = error "GG"

codeGenPrimitiveType :: PrimitiveType -> String
codeGenPrimitiveType Positive = mathbb "Z" ++ "^+"
codeGenPrimitiveType Natural = mathbb "N"
codeGenPrimitiveType Integer = mathbb "Z"
codeGenPrimitiveType Rational = mathbb "Q"
codeGenPrimitiveType Real = mathbb "R"
codeGenPrimitiveType Boolean = mathbb "B"

-- LATEX HELPERS --
textSep = "~"
newline = "\\\\\n"

cross = symbol $ macro "times"
arrow = symbol $ macro "to"

geq = symbol $ macro "geq"
leq = symbol $ macro "leq"

mathbb = macro1 "mathbb"
text = macro1 "text"

flalign = block "flalign*"
makecell contents = macro "makecell" ++ "{\n" ++ contents ++ "}"


macro :: String -> String
macro name = "\\" ++ name

macro1 :: String -> String -> String
macro1 name contents = macro name ++ "{" ++ contents ++ "}"

block :: String -> String -> String
block name contents = macro1 "begin" name ++ "\n" ++ contents ++ macro1 "end" name