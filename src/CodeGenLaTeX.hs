module CodeGenLaTeX (codeGenLaTeX, codeGenExpr) where

import Elab (ElabResult)
import Types (Fragment(..))
import IR (IR(..), IRDeclaration (IRDeclaration), IRImplementation (..), IRLocal (..), IRExpr (..), IRWhereTerm (IRLocalDecl, IRConstraint), IRBranch (..), IRBinaryOp (..), IRDeclAnnotations (..), IRBlockAnnotations (..))
import Data.List (intercalate)
import Data.List.NonEmpty (toList)
import AST (Signature (Signature), PrimitiveType (..), Type (..), UnaryOp (..), DeclDisplayMode (..), BlockDisplayMode (..))
import CodeGen
import qualified Data.Set as Set

makeCounterName :: String -> String
makeCounterName className = "nbm" ++ className ++ "Counter"

codeGenLaTeX :: ElabResult -> String
codeGenLaTeX (frags, _) = concatMap initCounter blockClasses ++ concat (map codeGenFragment frags)
    where   initCounter className = macro1 "newcounter" (makeCounterName className) ++ "\n"
        
            blockClasses = collectClasses [ans | (CodeFragment (IR ans _)) <- frags]

            codeGenFragment (TextFragment str) = str
            codeGenFragment (CodeFragment ir) = codeGenBlock ir

collectClasses :: [IRBlockAnnotations] -> Set.Set String
collectClasses anss = foldl insertClass Set.empty anss
    where   insertClass acc ans = Set.insert (blockClass ans) acc


codeGenBlock :: IR -> String
codeGenBlock (IR ans decls) = case displayMode of 
        HiddenBlock -> ""   -- omit blocks annotated with 'hidden'
        _ -> case intercalate dnl $ map (`codeGenDeclaration` innerDisplayMode) $ filter declVisible decls of 
                "" -> ""    -- don't wrap empty blocks
                res -> wrapBlock displayMode res
    where   dnl = doubleNewlineOrSpace displayMode  
            innerDisplayMode    | displayMode == BoxBlock = DefaultBlock    -- for simplicity, inner logic treats BoxBlock just like a DefaultBlock
                                | otherwise = displayMode
            displayMode = blockDisplayMode ans

            -- here we create a function to wrap a block, based on the display mode
            wrapBlock DefaultBlock = flalign
            wrapBlock BoxBlock = (`codeGenBox` ans) . flalign   -- wrap the block in a fancy box
            wrapBlock _ = id

            declVisible (IRDeclaration declAns _ _ _ _ _) = declDisplayMode declAns /= HiddenDecl

codeGenDeclaration :: IRDeclaration -> BlockDisplayMode -> String
codeGenDeclaration (IRDeclaration _ ident sig params impl whereTerms) blockDisplay =
    wrapOuter (txt "Define" ++ sep ++ wrapInner (ident ++ symbol ":" ++ codeGenSignature sig) ++ sep ++ txt "by") ++ nl ++
    wrap (ident ++ maybeParenTuple' params ++ symbol "=" ++ codeGenImpl impl ++ "." `insertIf` noWherePart) ++
    (maybeComma ++ nl ++ wrapOuter (
        txt "where" ++ sep ++ intercalateSpecialLast ", " (sep ++ txt "and" ++ sep) (map (wrapInner . codeGenWhereTerm) whereTerms) ++ "."
    )) `insertIf` not noWherePart
    where   (wrapOuter, wrapInner)  | blockDisplay == DefaultBlock = (wrap, id)
                                    | otherwise = (id, wrap)
        
            txt = textOrStr blockDisplay
            sep = sepOrSpace blockDisplay

            nl = newlineOrSpace blockDisplay
            wrap = wrapStatement blockDisplay

            maybeComma  | blockDisplay == InLineBlock = ","
                        | otherwise = ""

            noWherePart = null whereTerms

wrapStatement :: BlockDisplayMode -> String -> String
wrapStatement DefaultBlock str = "&" ++ str ++ "&"
wrapStatement _ str = "$" ++ str ++ "$"

textOrStr :: BlockDisplayMode -> String -> String
textOrStr DefaultBlock = text
textOrStr _ = id

sepOrSpace :: BlockDisplayMode -> String
sepOrSpace DefaultBlock = textSep
sepOrSpace _ = " "

newlineOrSpace :: BlockDisplayMode -> String
newlineOrSpace InLineBlock = " "
newlineOrSpace _ = newline

doubleNewlineOrSpace :: BlockDisplayMode -> String
doubleNewlineOrSpace InLineBlock = " "
doubleNewlineOrSpace _ = newline ++ newline

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

-- codeGenWhere :: [IRWhereTerm] -> BlockDisplayMode -> String
-- codeGenWhere [] _ = ""
-- codeGenWhere whereTerms blockDisplay
--     | blockDisplay == DefaultBlock = nl ++ wrap (txt "where" ++ sep ++ intercalateSpecialLast ", " (sep ++ txt "and" ++ sep) (map codeGenWhereTerm whereTerms) ++ ".")
--     | otherwise = maybeComma ++ nl ++ txt "where" ++ sep ++ intercalateSpecialLast ", " (sep ++ txt "and" ++ sep) (map (wrap . codeGenWhereTerm) whereTerms) ++ "."
--     where   txt = textOrStr blockDisplay
--             sep = sepOrSpace blockDisplay
        
--             nl = newlineOrSpace blockDisplay
--             wrap = wrapStatement blockDisplay
--             maybeComma  | blockDisplay == InLineBlock = ","
--                         | otherwise = ""
-- codeGenWhere whereTerms _ = text "where" ++ textSep ++ intercalateSpecialLast ", " (textSep ++ text "and" ++ textSep) (NonEmpty.map codeGenWhereTerm whereTerms)

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

codeGenBox :: String -> IRBlockAnnotations -> String
codeGenBox contents ans = 
    macro1 "refstepcounter" refCounterName ++ "\n" ++
    (\l -> macro1 "label" l) `insertIfJust` (blockLabel ans) ++ "\n" ++
    "{\n" ++
        macro "par" ++ "\n" ++
        macro "centering" ++ "\n" ++
            macro1 "fbox" ("\n" ++
                block1 "minipage" (Just "t") (macro "linewidth") (
                   vspace ++ "\n" ++
                   macro1 "textbf" (blockClass ans ++ " " ++ macro ("the" ++ refCounterName) ++ (\n -> ": " ++ n) `insertIfJust` blockName ans) ++ "\n" ++
                   (\d -> newline ++ "{" ++ macro "small" ++ " " ++ d ++ "}") `insertIfJust` blockDescription ans ++
                   vspace ++ "\n" ++
                   macro "hrule" ++ "\n" ++
                   macro1 "vspace" ("-" ++ macro "abovedisplayskip") ++ "\n" ++
                   contents
                )
            ++ "\n") ++ "\n" ++
        vspace ++ "\n" ++
        macro "par" ++
    "\n}"
    where   refCounterName = makeCounterName (blockClass ans)
            vspace = macro1 "vspace" "0.5em"

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
block name contents = macro1 "begin" name ++ "\n" ++ contents ++ "\n" ++ macro1 "end" name

block1 :: String -> Maybe String -> String -> String -> String
block1 name option arg contents = macro1 "begin" name ++ (\o -> "[" ++ o ++ "]") `insertIfJust` option ++ "{" ++ arg ++ "}" ++ "\n" ++ contents ++ "\n" ++ macro1 "end" name