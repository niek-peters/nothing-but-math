-- | LaTeX code generation. Takes evaluation phase output and produces LaTeX.
module CodeGenLaTeX (codeGenLaTeX, codeGenExpr) where

import Types (Fragment(..))
import IR (IR(..), IRDeclaration (IRDeclaration), IRImplementation (..), IRLocal (..), IRExpr (..), IRWhereTerm (IRLocalDecl, IRConstraint), IRBranch (..), IRBinaryOp (..), IRDeclAnnotations (..), IRBlockAnnotations (..), IREvalResult (..))
import Data.List (intercalate)
import Data.List.NonEmpty (toList)
import qualified Data.List.NonEmpty as NonEmpty
import AST (Signature (Signature), Type (..), DeclDisplayMode (..), BlockDisplayMode (..))
import CodeGen
import qualified Data.Set as Set
import Eval (EvalResult)
import Token (UnaryOp(..), PrimitiveType (..))

-- | Construct a LaTeX counter name for a given block class.
makeCounterName :: String -> String
makeCounterName className = "nbm" ++ className ++ "Counter"

-- | Generate LaTeX document fragments from evaluation fragments.
codeGenLaTeX :: EvalResult -> String
codeGenLaTeX frags = concatMap initCounter blockClasses ++ concat (map codeGenFragment frags)
    where   initCounter className = macro1 "newcounter" (makeCounterName className) ++ "\n"
        
            blockClasses = collectClasses [ans | (DefinitionFragment (IR ans _)) <- frags]

            codeGenFragment (TextFragment str) = str
            codeGenFragment (DefinitionFragment ir) = codeGenBlock ir
            codeGenFragment (EvalFragment e) = codeGenEvalRes e

-- | Collect all distinct block classes from block annotations.
collectClasses :: [IRBlockAnnotations] -> Set.Set String
collectClasses anss = foldl insertClass Set.empty anss
    where   insertClass acc ans = Set.insert (blockClass ans) acc

-- | Render a complete IR block (with annotations) to LaTeX, skipping hidden blocks.
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

-- | Render a declaration inside a block taking the block display mode into account.
codeGenDeclaration :: IRDeclaration -> BlockDisplayMode -> String
codeGenDeclaration (IRDeclaration _ ident sig params impl whereTerms) blockDisplay =
    wrapOuter (txt "Define" ++ sep ++ wrapInner (ident ++ symbol ":" ++ codeGenSignature sig) ++ sep ++ txt "by") ++ nl ++
        wrapStmt (ident ++ maybeParenTuple' params ++ symbol "=" ++ codeGenImpl impl ++ "." `insertIf` noWherePart) ++
    (maybeComma ++ nl ++ wrapOuter (
        txt "where" ++ sep ++ intercalateSpecialLast ", " (sep ++ txt "and" ++ sep) (map (wrapInner . codeGenWhereTerm) whereTerms) ++ "."
    )) `insertIf` not noWherePart
    where   (wrapOuter, wrapInner)  | blockDisplay == DefaultBlock = (wrapStmt, id)
                                    | otherwise = (id, wrapStmt)
        
            txt = textOrStr blockDisplay
            sep = sepOrSpace blockDisplay

            nl = newlineOrSpace blockDisplay
            wrapStmt = wrapStatement blockDisplay

            maybeComma  | blockDisplay == InLineBlock = ","
                        | otherwise = ""

            noWherePart = null whereTerms

-- | Wrap a statement appropriately for the given block display mode.
wrapStatement :: BlockDisplayMode -> String -> String
wrapStatement DefaultBlock str = "&" ++ str ++ "&"
wrapStatement _ str = "$" ++ str ++ "$"

-- | For block text rendering, return either the `text` macro or identity based on mode.
textOrStr :: BlockDisplayMode -> String -> String
textOrStr DefaultBlock = text
textOrStr _ = id

-- | Separator used between textual pieces depending on block mode.
sepOrSpace :: BlockDisplayMode -> String
sepOrSpace DefaultBlock = textSep
sepOrSpace _ = " "

-- | Either newline or space depending on whether the block is inline.
newlineOrSpace :: BlockDisplayMode -> String
newlineOrSpace InLineBlock = " "
newlineOrSpace _ = newline

-- | Two newlines or a single space for inline mode; used between declarations.
doubleNewlineOrSpace :: BlockDisplayMode -> String
doubleNewlineOrSpace InLineBlock = " "
doubleNewlineOrSpace _ = newline ++ newline

-- | Convert a `Signature` to a LaTeX representation of types.
codeGenSignature :: Signature -> String
codeGenSignature (Signature maybeFromT (Type tos)) = fromPart ++ toPart
    where   fromPart = case maybeFromT of
                Nothing -> ""
                Just (Type froms) -> typeTuple froms ++ arrow
            toPart = typeTuple tos

            typeTuple pts = intercalate cross (map codeGenPrimitiveType (toList pts)) 

-- | Render an IR implementation for LaTeX (either a single expression or a cases block).
codeGenImpl :: IRImplementation -> String
codeGenImpl (IRUnconditional e) = codeGenExpr e
codeGenImpl (IRConditional branches other) = block "cases" $ intercalate newline (map codeGenBranch branches ++ [codeGenOther other])-- concat (map codeGenBranch branches) ++ codeGenOther other

-- | Render a single case branch for LaTeX output.
codeGenBranch :: IRBranch -> String
codeGenBranch (IRBranch e cond) = tab ++ codeGenExpr e ++ symbol "&" ++ text "if" ++ textSep ++ codeGenExpr cond

-- | Render the `otherwise` clause for a cases block.
codeGenOther :: IRExpr -> String
codeGenOther e = tab ++ codeGenExpr e ++ symbol "&" ++ text "otherwise"

-- | Render a where-term (local declaration or constraint) for LaTeX.
codeGenWhereTerm :: IRWhereTerm -> String
codeGenWhereTerm (IRLocalDecl (IRLocal idents e)) = maybeParenTuple idents ++ symbol "=" ++ codeGenExpr e
codeGenWhereTerm (IRConstraint e) = codeGenExpr e

-- | Render an evaluation result (expr = result) for inline LaTeX output.
codeGenEvalRes :: IREvalResult -> String
codeGenEvalRes (IREvalResult e1 e2) = wrapStatement InLineBlock $ codeGenExpr e1 ++ symbol "=" ++ codeGenExpr e2

-- | Render an `IRExpr` to its LaTeX string representation.
codeGenExpr :: IRExpr -> String
codeGenExpr (IRCast e _ _) = codeGenExpr e
codeGenExpr (IRCall ident _ es) = ident ++ maybeParenTuple' (map codeGenExpr es)
codeGenExpr (IRImmediateInt i _) = show i
codeGenExpr (IRImmediateReal r) = show r
codeGenExpr (IRImmediateBool b) = show b
codeGenExpr (IRBinary op e1 e2) = codeGenBinary op (unwrapCast e1) (unwrapCast e2)
codeGenExpr (IRUnary op e) = codeGenUnary op (unwrapCast e)
codeGenExpr (IRTuple es) = parenTuple $ map codeGenExpr (toList es)

-- | Remove an `IRCast` wrapper if present.
unwrapCast :: IRExpr -> IRExpr
unwrapCast (IRCast e _ _) = e
unwrapCast e = e

-- | Conditionally parenthesize a child expression depending on operator precedence and associativity.
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

-- | Render unary operations to LaTeX, applying parentheses where necessary.
codeGenUnary :: UnaryOp -> IRExpr -> String
codeGenUnary Neg e = "-" ++ (maybeParens e (Left Neg) False $ codeGenExpr e)
codeGenUnary Floor e = wrap (macro "lfloor") (macro "rfloor")  $ codeGenExpr e
codeGenUnary Sqrt e = macro1 "sqrt"  $ codeGenExpr e
codeGenUnary Not e = macro "neg " ++ (maybeParens e (Left Not) False $ codeGenExpr e)

-- | Render binary IR operations to LaTeX, choosing the appropriate macro or operator.
codeGenBinary :: IRBinaryOp -> IRExpr -> IRExpr -> String
codeGenBinary op@IRAdd = infixOp "+" op
codeGenBinary op@IRSub = infixOp "-" op
codeGenBinary op@IRMult = infixOp (macro "cdot") op
codeGenBinary IRFrac = fracOp
codeGenBinary IRDiv = fracOp
codeGenBinary IRPosPow = powerOp
codeGenBinary IRFracPow = powerOp
codeGenBinary IRFloatPow = powerOp
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

-- | Helper for binary infix rendering with parentheses according to precedence.
infixOp :: String -> IRBinaryOp -> IRExpr -> IRExpr -> String
infixOp s op e1 e2 = (maybeParens e1 (Right op) False $ codeGenExpr e1) ++ symbol s ++ (maybeParens e2 (Right op) True $ codeGenExpr e2)

-- | Render a LaTeX `\frac{num}{den}` from two expressions.
fracOp :: IRExpr -> IRExpr -> String
fracOp e1 e2 = macro2 "frac" (codeGenExpr e1) (codeGenExpr e2)

-- | Render power expressions with braces for the exponent.
powerOp :: IRExpr -> IRExpr -> String
powerOp e1 e2 = (maybeParens e1 (Right IRPosPow) False $ codeGenExpr e1) ++ "^{" ++ (codeGenExpr e2) ++ "}"

-- | Map primitive types to their LaTeX mathbb representations.
codeGenPrimitiveType :: PrimitiveType -> String
codeGenPrimitiveType Positive = mathbb "Z" ++ "^+"
codeGenPrimitiveType Natural = mathbb "N"
codeGenPrimitiveType Integer = mathbb "Z"
codeGenPrimitiveType Rational = mathbb "Q"
codeGenPrimitiveType Real = mathbb "R"
codeGenPrimitiveType Boolean = mathbb "B"

-- | Emit a fancy LaTeX box for a block with optional niceties like label and description.
codeGenBox :: String -> IRBlockAnnotations -> String
codeGenBox contents ans = 
    macro1 "refstepcounter" refCounterName ++ "\n" ++
    (\l -> macro1 "label" l) `insertIfJust` (blockLabel ans) ++ "\n" ++
    "{\n" ++
        macro "par" ++ "\n" ++
        macro "centering" ++ "\n" ++
            macro1 "fbox" ("%\n" ++
                block1 "minipage" (Just "t") (macro "dimexpr" ++ macro "linewidth" ++ "-2" ++ macro "fboxsep" ++ "-2" ++ macro "fboxrule") (
                   vspace ++ "\n" ++
                   macro1 "textbf" (blockClass ans ++ " " ++ macro ("the" ++ refCounterName) ++ (\n -> ": " ++ n) `insertIfJust` blockName ans) ++ "\n" ++
                   (\d -> newline ++ "{" ++ macro "small" ++ " " ++ d ++ "}") `insertIfJust` blockDescription ans ++
                   vspace ++ "\n" ++
                   macro "hrule" ++ "\n" ++
                   macro1 "vspace" ("-" ++ macro "abovedisplayskip") ++ "\n" ++
                   contents
                )
            ++ "%\n") ++ "\n" ++
        vspace ++ "\n" ++
        macro "par" ++
    "\n}"
    where   refCounterName = makeCounterName (blockClass ans)
            vspace = macro1 "vspace" "0.5em"

-- LATEX HELPERS --
-- | Non-breaking text separator macro.
textSep :: String
textSep = "~"

-- | Newline marker for LaTeX.
newline :: String
newline = "\\\\\n"

-- | Times operator in LaTeX math mode.
cross :: String
cross = symbol $ macro "times"
-- | Arrow operator in LaTeX math mode.
arrow :: String
arrow = symbol $ macro "to"

-- | Convenience to emit `\mathbb{...}` with a single argument.
mathbb :: String -> String
mathbb = macro1 "mathbb"
-- | Convenience to emit `\text{...}` with a single argument.
text :: String -> String
text = macro1 "text"

-- | flalign environment helper.
flalign :: String -> String
flalign = block "flalign*"

-- | Tuple/parenthesis helpers using LaTeX-sized delimiters.
maybeParenTuple :: NonEmpty.NonEmpty String -> String
maybeParenTuple = maybeTuple (macro "left(") (macro "right)")

maybeParenTuple' :: [String] -> String
maybeParenTuple' = maybeTuple' (macro "left(") (macro "right)")

parenTuple :: [String] -> String
parenTuple = tuple (macro "left(") (macro "right)")

parens :: String -> String
parens = wrap (macro "left(") (macro "right)")

-- | Create a LaTeX macro name like `\name`.
macro :: String -> String
macro name = "\\" ++ name

-- | Emit a LaTeX macro with one argument: `\name{arg}`.
macro1 :: String -> String -> String
macro1 name arg = macro name ++ "{" ++ arg ++ "}"

-- | Emit a LaTeX macro with two arguments: `\name{arg1}{arg2}`.
macro2 :: String -> String -> String -> String
macro2 name arg1 arg2 = macro name ++ "{" ++ arg1 ++ "}{" ++ arg2 ++ "}"

-- | Helper for emitting LaTeX begin/end blocks with contents.
block :: String -> String -> String
block name contents = macro1 "begin" name ++ "\n" ++ contents ++ "\n" ++ macro1 "end" name

-- | Helper for begin with optional argument and required second argument.
block1 :: String -> Maybe String -> String -> String -> String
block1 name option arg contents = macro1 "begin" name ++ (\o -> "[" ++ o ++ "]") `insertIfJust` option ++ "{" ++ arg ++ "}" ++ "\n" ++ contents ++ "\n" ++ macro1 "end" name