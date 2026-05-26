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

-- here we handle different LaTeX block types
-- we wrap the result in a certain block type and pass a statement wrapper function
codeGenBlock :: IRDeclaration -> BlockType -> String
codeGenBlock decl bt = wrapBlock bt $ codeGenDeclaration decl (wrapStatement bt)

wrapBlock :: BlockType -> String -> String
wrapBlock Text = flalign
wrapBlock Table = makecell

wrapStatement :: BlockType -> String -> String
wrapStatement Text str = "&" ++ str ++ "&"
wrapStatement Table str = "$" ++ str ++ "$"

codeGenDeclaration :: IRDeclaration -> (String -> String) -> String
codeGenDeclaration (IRDeclaration ident sig params impl locals constraints) wrap = 
    wrap (text "Define" ++ textSep ++ ident ++ " : " ++ codeGenSignature sig ++ textSep ++ text "by") ++ newline ++
    wrap (ident ++ codeGenParams params ++ " = " ++ codeGenImpl impl) ++ newline ++
    codeGenWhere locals constraints wrap

codeGenSignature :: Signature -> String
codeGenSignature _ = error "GG"

codeGenImpl :: IRImplementation -> String
codeGenImpl _ = error "GG"

-- NOTE: the original order of locals and decls is lost in the AST/IR. It might be a good idea to save those somehow
codeGenWhere :: [IRLocal] -> [IRExpr] -> (String -> String) -> String
codeGenWhere [] [] _ = ""
codeGenWhere locals es wrap = wrap ("GG") ++ error "GG" 


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

flalign = block "flalign*"
makecell = macro1 "makecell"


macro :: String -> String
macro name = "\\" ++ name

macro1 :: String -> String -> String
macro1 name contents = "\\" ++ name ++ "{" ++ contents ++ "}"

block :: String -> String -> String
block name contents = macro1 "begin" name ++ "\n" ++ contents ++ "\n" ++ macro1 "end" name