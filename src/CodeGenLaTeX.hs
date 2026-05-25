module CodeGenLaTeX (codeGenLaTeX) where

import Elab (ElabResult)
import Types (Fragment(..))
import IR (IR(..), IRDeclaration (IRDeclaration), IRImplementation, IRLocal, IRExpr)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import AST (Signature)

data BlockType = Text | Table

codeGenLaTeX :: ElabResult -> String
codeGenLaTeX frags = concat (map codeGenFragment frags)
    where   
            codeGenFragment (TextFragment str) = str
            codeGenFragment (CodeFragment (IR decls)) = concat (map (`codeGenBlock` Text) decls)    -- TODO: remove hardcoded Text and get it from DSL annotation 

codeGenBlock :: IRDeclaration -> BlockType -> String
codeGenBlock decl Text = "\\begin{flalign*}\n" ++ codeGenDeclaration decl Text ++ "\\end{flalign*}\n"
codeGenBlock decl Table = "\\makecell{\n" ++ codeGenDeclaration decl Table ++ "}\n"

codeGenDeclaration :: IRDeclaration -> BlockType -> String
codeGenDeclaration (IRDeclaration ident sig params impl locals constraints) bt = 
    wrap (text "Define" ++ textSep ++ ident ++ " : " ++ codeGenSignature sig ++ textSep ++ text "by") ++ newline ++
    wrap (ident ++ codeGenParams params ++ " = " ++ codeGenImpl impl) ++ newline ++
    codeGenWhere locals constraints wrap ++
    error "GG"

    where   wrap = wrapStatement bt

codeGenSignature :: Signature -> String
codeGenSignature _ = error "GG"

codeGenImpl :: IRImplementation -> String
codeGenImpl _ = error "GG"

-- NOTE: the original order of locals and decls is lost in the AST/IR. It might be a good idea to save those somehow
codeGenWhere :: [IRLocal] -> [IRExpr] -> (String -> String) -> String
codeGenWhere [] [] _ = ""
codeGenWhere locals es wrap = wrap ("GG") ++ error "GG" 

wrapStatement :: BlockType -> String -> String
wrapStatement Text str = "&" ++ str ++ "&"
wrapStatement Table str = "$" ++ str ++ "$"

codeGenParams :: [String] -> String
codeGenParams [] = ""
codeGenParams els = parens $ intercalate ", " els

-- maybeTuple :: [String] -> String
-- maybeTuple [] = ""
-- maybeTuple [el] = el
-- maybeTuple els = parens $ intercalate ", " els

parens :: String -> String
parens str = "(" ++ str ++ ")"

-- LATEX HELPERS --
textSep = "~"
newline = "\\\\"

times = macro "times"
to = macro "to"
geq = macro "geq"
leq = macro "leq"

mathbb = macro1 "mathbb"
text = macro1 "text"

macro :: String -> String
macro name = "\\" ++ name

macro1 :: String -> String -> String
macro1 name contents = "\\" ++ name ++ "{" ++ contents ++ "}"
