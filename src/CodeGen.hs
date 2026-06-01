module CodeGen (module CodeGen) where
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty

-- Generic codeGen helpers --

-- wraps a string in single spaces
symbol :: String -> String
symbol s = " " ++ s ++ " "

-- either a single element or a tuple
maybeTuple :: String -> String -> NonEmpty String -> String
maybeTuple _ _ (el :| []) = el
maybeTuple symLeft symRight els = tuple symLeft symRight (toList els)

-- either nothing or tuple
maybeTuple' :: String -> String -> [String] -> String
maybeTuple' _ _ [] = ""
maybeTuple' symLeft symRight els = tuple symLeft symRight els

tuple :: String -> String -> [String] -> String
tuple symLeft symRight els = wrap symLeft symRight $ intercalate ", " els

intercalateSpecialLast :: String -> String -> [String] -> String
intercalateSpecialLast _ _ [] = ""
intercalateSpecialLast _ _ [str] = str
intercalateSpecialLast sep lastSep strs = (intercalate sep $ init (strs)) ++ lastSep ++ last strs

wrap :: String -> String -> String -> String
wrap symLeft symRight str = symLeft ++ str ++ symRight

-- removes outer symbols if present
unwrap :: String -> String -> String -> String
unwrap symLeft symRight str | length str < combinedLength = str
                            | ',' `elem` str = str  -- don't unwrap tuples
                            | prefix == symLeft && postfix == symRight = rest2
                            | otherwise = str       -- don't unwrap if not wrapped
                    
    where   (prefix, rest1) = splitAt symbolLeftLength str
            (rest2, postfix) = splitAt (length str - combinedLength) rest1
            
            combinedLength = symbolLeftLength + symbolRightLength
            symbolLeftLength = length symLeft
            symbolRightLength = length symRight

-- -- removes outer parentheses if present
-- -- intended for use around codeGenExpr
-- unparens :: String -> String
-- unparens str    | length str < 2 = str
--                 | ',' `elem` str = str  -- don't remove parens from tuples
--                 | hasOpenParen && hasCloseParen = (init . tail) str
--                 | otherwise = str
--     where   hasOpenParen = head str == '('
--             hasCloseParen = last str == ')'

infixBinaryOp :: (a -> String) -> String -> a -> a -> String
infixBinaryOp gen opStr e1 e2 = gen e1 ++ symbol opStr ++ gen e2

insertIf :: String -> Bool -> String
insertIf str True = str
insertIf _ False = ""

tab = "  "
