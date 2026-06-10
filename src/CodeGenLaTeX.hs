module CodeGenLaTeX (codeGenLaTeX, codeGenExpr) where

import Types (Fragment(..))
import IR (IR(..), IRDeclaration (IRDeclaration), IRImplementation (..), IRLocal (..), IRExpr (..), IRWhereTerm (IRLocalDecl, IRConstraint), IRBranch (..), IRBinaryOp (..), IRDeclAnnotations (..), IRBlockAnnotations (..), IREvalResult (..))
import Data.List (intercalate)
import Data.List.NonEmpty (toList)
import AST (Signature (Signature), Type (..), DeclDisplayMode (..), BlockDisplayMode (..))
import CodeGen
import qualified Data.Set as Set
import Eval (EvalResult)
import Token (UnaryOp(..), PrimitiveType (..))

makeCounterName :: String -> String
makeCounterName className = "nbm" ++ className ++ "Counter"

codeGenLaTeX :: EvalResult -> String
codeGenLaTeX frags = concatMap initCounter blockClasses ++ concat (map codeGenFragment frags)
    where   initCounter className = macro1 "newcounter" (makeCounterName className) ++ "\n"
        
            blockClasses = collectClasses [ans | (DefinitionFragment (IR ans _)) <- frags]

            codeGenFragment (TextFragment str) = str
            codeGenFragment (DefinitionFragment ir) = codeGenBlock ir
            codeGenFragment (EvalFragment e) = codeGenEvalRes e

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
codeGenImpl (IRUnconditional e) = codeGenExpr e
codeGenImpl (IRConditional branches other) = block "cases" $ intercalate newline (map codeGenBranch branches ++ [codeGenOther other])-- concat (map codeGenBranch branches) ++ codeGenOther other

codeGenBranch :: IRBranch -> String
codeGenBranch (IRBranch e cond) = tab ++ codeGenExpr e ++ ", & " ++ text "if" ++ textSep ++ codeGenExpr cond

codeGenOther :: IRExpr -> String
codeGenOther e = tab ++ codeGenExpr e ++ ", & " ++ text "otherwise"

codeGenWhereTerm :: IRWhereTerm -> String
codeGenWhereTerm (IRLocalDecl (IRLocal idents e)) = maybeParenTuple idents ++ symbol "=" ++ codeGenExpr e
codeGenWhereTerm (IRConstraint e) = codeGenExpr e

codeGenEvalRes :: IREvalResult -> String
codeGenEvalRes (IREvalResult e1 e2) = wrapStatement InLineBlock $ codeGenExpr e1 ++ symbol "=" ++ codeGenExpr e2

codeGenExpr :: IRExpr -> String
codeGenExpr (IRCast e _ _) = codeGenExpr e
codeGenExpr (IRCall ident _ es) = ident ++ maybeParenTuple' (map codeGenExpr es)
codeGenExpr (IRImmediateInt i _) = show i
codeGenExpr (IRImmediateReal r) = show r
codeGenExpr (IRImmediateBool b) = show b
codeGenExpr (IRBinary op e1 e2) = codeGenBinary op (unwrapCast e1) (unwrapCast e2)
codeGenExpr (IRUnary op e) = codeGenUnary op (unwrapCast e)
codeGenExpr (IRTuple es) = parenTuple $ map codeGenExpr (toList es)

unwrapCast :: IRExpr -> IRExpr
unwrapCast (IRCast e _ _) = e
unwrapCast e = e

maybeParens :: IRExpr -> Either UnaryOp IRBinaryOp -> Bool -> String -> String
maybeParens (IRBinary op _ _) parentOp isRight   
    | childOpLevel < parentOpLevel = parens
    | childOpLevel == parentOpLevel && isRight = parens
    | otherwise = id
    where   childOpLevel = opLevel $ Right op
            parentOpLevel = opLevel parentOp
maybeParens (IRUnary op _) parentOp _
    | (opLevel $ Left op) < (opLevel parentOp) = parens
    | otherwise = id
maybeParens _ _ _ = id

codeGenUnary :: UnaryOp -> IRExpr -> String
codeGenUnary Neg e = "-" ++ (maybeParens e (Left Neg) False $ codeGenExpr e)
codeGenUnary Floor e = wrap (macro "lfloor") (macro "rfloor")  $ codeGenExpr e
codeGenUnary Sqrt e = macro1 "sqrt"  $ codeGenExpr e
codeGenUnary Not e = macro "neg " ++ (maybeParens e (Left Not) False $ codeGenExpr e)

codeGenBinary :: IRBinaryOp -> IRExpr -> IRExpr -> String
codeGenBinary op@IRAdd = infixOp "+" op
codeGenBinary op@IRSub = infixOp "-" op
codeGenBinary op@IRMult = infixOp (macro "cdot") op
codeGenBinary IRFrac = fracOp
codeGenBinary IRDiv = fracOp
codeGenBinary IRPow = powerOp
codeGenBinary IRExp = powerOp
codeGenBinary op@IRMod = infixOp (macro "bmod") op
codeGenBinary op@IREq = infixOp "=" op
codeGenBinary op@IRNeq = infixOp (macro "neq") op
codeGenBinary op@IRLess = infixOp "<" op
codeGenBinary op@IRGreater = infixOp ">" op
codeGenBinary op@IRLessEq = infixOp (macro "leq") op
codeGenBinary op@IRGreaterEq = infixOp (macro "geq") op
codeGenBinary op@IRDivides = infixOp (macro "mid") op
codeGenBinary op@IRAnd = infixOp (macro "land") op
codeGenBinary op@IROr = infixOp (macro "lor") op

infixOp :: String -> IRBinaryOp -> IRExpr -> IRExpr -> String
infixOp s op e1 e2 = (maybeParens e1 (Right op) False $ codeGenExpr e1) ++ symbol s ++ (maybeParens e2 (Right op) True $ codeGenExpr e2)


fracOp :: IRExpr -> IRExpr -> String
fracOp e1 e2 = macro2 "frac" (codeGenExpr e1) (codeGenExpr e2)

powerOp :: IRExpr -> IRExpr -> String
powerOp e1 e2 = (maybeParens e1 (Right IRPow) False $ codeGenExpr e1) ++ "^{" ++ (codeGenExpr e2) ++ "}"

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

maybeParenTuple = maybeTuple (macro "left(") (macro "right)")
maybeParenTuple' = maybeTuple' (macro "left(") (macro "right)")
parenTuple = tuple (macro "left(") (macro "right)")
parens = wrap (macro "left(") (macro "right)")

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