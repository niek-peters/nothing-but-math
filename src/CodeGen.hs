module CodeGen (module CodeGen) where
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty

-- Generic codeGen helpers --

-- wraps a string in single spaces
symbol :: String -> String
symbol s = " " ++ s ++ " "

-- either a single element or a tuple
maybeTuple :: NonEmpty String -> String
maybeTuple (el :| []) = el
maybeTuple els = tuple (toList els)

-- either nothing or tuple
maybeTuple' :: [String] -> String
maybeTuple' [] = ""
maybeTuple' els = tuple els

tuple :: [String] -> String
tuple els = parens $ intercalate ", " els

intercalateSpecialLast :: String -> String -> NonEmpty String -> String
intercalateSpecialLast _ _ (str :| []) = str
intercalateSpecialLast sep lastSep strs = (intercalate sep $ NonEmpty.init (strs)) ++ lastSep ++ NonEmpty.last strs

parens :: String -> String
parens str = "(" ++ str ++ ")"

-- removes outer parentheses if present
-- intended for use around codeGenExpr
unparens :: String -> String
unparens str    | length str < 2 = str
                | ',' `elem` str = str  -- don't remove parens from tuples
                | hasOpenParen && hasCloseParen = (init . tail) str
                | otherwise = str
    where   hasOpenParen = head str == '('
            hasCloseParen = last str == ')'

insertIf :: String -> Bool -> String
insertIf str True = str
insertIf _ False = ""

tab = "  "
