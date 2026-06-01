module CodeGenLaTeX (codeGenLaTeX) where

import Elab (ElabResult)
import Types (Fragment(..))
import IR (IR(..), IRDeclaration (IRDeclaration), IRImplementation (..), IRLocal (..), IRExpr (..), IRWhereTerm (IRLocalDecl, IRConstraint), IRBranch (..), IRBinaryOp (..))
import Data.List (intercalate)
import Data.List.NonEmpty (toList, fromList, NonEmpty)
import AST (Signature (Signature), PrimitiveType (..), Type (..), UnaryOp (..))
import CodeGen
import qualified Data.List.NonEmpty as NonEmpty

-- NOTE: these 'blocks' should probably have the same boundaries as the DSL '<<<' '>>>' blocks
-- the annotations should probably be on the block level too

data BlockType  = Default   -- outputs block in \begin{flalign*}...\end{flalign*} block
                | InText    -- outputs block as in-text lines wrapped with $

-- TODO: remove hardcoded Text and get it from DSL annotation 
hardCodedBlockType :: BlockType
hardCodedBlockType = Default

codeGenLaTeX :: ElabResult -> String
codeGenLaTeX frags = concat (map codeGenFragment frags)
    where   codeGenFragment (TextFragment str) = str
            -- TODO: use annotations
            codeGenFragment (CodeFragment (IR blockAns decls)) = wrapBlock hardCodedBlockType $ intercalate newline $ map (`codeGenBlock` hardCodedBlockType) decls  

            -- here we handle different LaTeX block types
            wrapBlock Default = flalign
            wrapBlock InText = id

-- we pass a statement wrapper function based on the block type
codeGenBlock :: IRDeclaration -> BlockType -> String
codeGenBlock decl bt = codeGenDeclaration decl (wrapStatement bt)
    -- where   wrapBlock Default = flalign
    --         wrapBlock InText = id

wrapStatement :: BlockType -> String -> String
wrapStatement Default str = "&" ++ str ++ "&"
wrapStatement InText str = "$" ++ str ++ "$"

-- TODO: use annotations
codeGenDeclaration :: IRDeclaration -> (String -> String) -> String
codeGenDeclaration (IRDeclaration declAns ident sig params impl whereTerms) wrap = 
    wrap (text "Define" ++ textSep ++ ident ++ symbol ":" ++ codeGenSignature sig ++ textSep ++ text "by") ++ newline ++
    wrap (ident ++ maybeParenTuple' params ++ symbol "=" ++ codeGenImpl impl ++ "." `insertIf` noWherePart) ++ newline ++
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
codeGenImpl (IRUnconditional e) = unparens $ codeGenExpr e
codeGenImpl (IRConditional branches other) = block "cases" $ intercalate newline (map codeGenBranch branches ++ [codeGenOther other])-- concat (map codeGenBranch branches) ++ codeGenOther other

codeGenBranch :: IRBranch -> String
codeGenBranch (IRBranch e cond) = tab ++ unparens (codeGenExpr e) ++ ", & " ++ text "if" ++ textSep ++ unparens (codeGenExpr cond)

codeGenOther :: IRExpr -> String
codeGenOther e = tab ++ unparens (codeGenExpr e) ++ ", & " ++ text "otherwise"

codeGenWhere :: NonEmpty IRWhereTerm -> String
codeGenWhere whereTerms = text "where" ++ textSep ++ intercalateSpecialLast ", " (textSep ++ text "and" ++ textSep) (NonEmpty.map codeGenWhereTerm whereTerms)

codeGenWhereTerm :: IRWhereTerm -> String
codeGenWhereTerm (IRLocalDecl (IRLocal idents e)) = maybeParenTuple idents ++ symbol "=" ++ (unparens $ codeGenExpr e)
codeGenWhereTerm (IRConstraint e) = unparens $ codeGenExpr e

codeGenExpr :: IRExpr -> String
codeGenExpr (IRCast e _ _) = codeGenExpr e
codeGenExpr (IRCall ident _ es) = ident ++ maybeParenTuple' (map (unparens . codeGenExpr) es)
codeGenExpr (IRImmediateInt i _) = show i
codeGenExpr (IRImmediateReal r) = show r
codeGenExpr (IRImmediateBool b) = show b
codeGenExpr (IRBinary op e1 e2) = maybeParens $ codeGenBinary op e1 e2
    where   maybeParens | op == IRDiv || op == IRFrac = id
                        | otherwise = parens
codeGenExpr (IRUnary op e) = codeGenUnary op e
codeGenExpr (IRTuple es) = parenTuple $ map (unparens . codeGenExpr) (toList es)

codeGenUnary :: UnaryOp -> IRExpr -> String
codeGenUnary Floor e = wrap (macro "lfloor") (macro "rfloor") $ unparens $ codeGenExpr e
codeGenUnary Sqrt e = macro1 "sqrt" $ unparens $ codeGenExpr e

codeGenBinary :: IRBinaryOp -> IRExpr -> IRExpr -> String
codeGenBinary IRAdd = infixOp "+"
codeGenBinary IRSub = infixOp "-"
codeGenBinary IRMult = infixOp (macro "cdot")
codeGenBinary IRFrac = fracOp
codeGenBinary IRDiv = fracOp
codeGenBinary IRPow = powerOp
codeGenBinary IRExp = powerOp
codeGenBinary IRMod = infixOp (macro "bmod")
codeGenBinary IREq = infixOp "="
codeGenBinary IRNeq = infixOp (macro "neq")
codeGenBinary IRLess = infixOp "<"
codeGenBinary IRGreater = infixOp ">"
codeGenBinary IRLessEq = infixOp (macro "leq")
codeGenBinary IRGreaterEq = infixOp (macro "geq")
codeGenBinary IRDivides = infixOp (macro "mid")

fracOp :: IRExpr -> IRExpr -> String
fracOp e1 e2 = macro2 "frac" (unparens $ codeGenExpr e1) (unparens $ codeGenExpr e2)

powerOp :: IRExpr -> IRExpr -> String
powerOp e1 e2 = (codeGenExpr e1) ++ "^{" ++ (unparens $ codeGenExpr e2) ++ "}"

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

mathbb = macro1 "mathbb"
text = macro1 "text"

flalign = block "flalign*"
-- makecell contents = macro "makecell" ++ "{\n" ++ contents ++ "}"

infixOp = infixBinaryOp codeGenExpr
maybeParenTuple = maybeTuple (macro "left(") (macro "right)")
maybeParenTuple' = maybeTuple' (macro "left(") (macro "right)")
parenTuple = tuple (macro "left(") (macro "right)")
parens = wrap (macro "left(") (macro "right)")
unparens = unwrap (macro "left(") (macro "right)")

macro :: String -> String
macro name = "\\" ++ name

macro1 :: String -> String -> String
macro1 name arg = macro name ++ "{" ++ arg ++ "}"

macro2 :: String -> String -> String -> String
macro2 name arg1 arg2 = macro name ++ "{" ++ arg1 ++ "}{" ++ arg2 ++ "}"

block :: String -> String -> String
block name contents = macro1 "begin" name ++ "\n" ++ contents ++ macro1 "end" name