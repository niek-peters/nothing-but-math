-- | Module exporting some common utilities used in both code generation targets
module CodeGen (module CodeGen) where
        
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List (intercalate)
import IR (IRBinaryOp (..))
import GHC.Base (maxInt)
import Token (UnaryOp (..))

-- Generic codeGen helpers --

-- | Wrap a string with single leading and trailing spaces.
symbol :: String -> String
symbol s = " " ++ s ++ " "

-- | Render a non-empty list as either a single element or a tuple using the given delimiters.
maybeTuple :: String -> String -> NonEmpty String -> String
maybeTuple _ _ (el :| []) = el
maybeTuple symLeft symRight els = tuple symLeft symRight (toList els)

-- | Render an optional list as a tuple; returns empty string for empty lists.
maybeTuple' :: String -> String -> [String] -> String
maybeTuple' _ _ [] = ""
maybeTuple' symLeft symRight els = tuple symLeft symRight els

-- | Join a list of strings with commas and wrap with the given delimiters.
tuple :: String -> String -> [String] -> String
tuple symLeft symRight els = wrap symLeft symRight $ intercalate ", " els

-- | Intercalate a list using `sep` between all but the last two elements, and `lastSep` before the final element.
intercalateSpecialLast :: String -> String -> [String] -> String
intercalateSpecialLast _ _ [] = ""
intercalateSpecialLast _ _ [str] = str
intercalateSpecialLast sep lastSep strs = (intercalate sep $ init (strs)) ++ lastSep ++ last strs

-- | Surround `str` with `symLeft` and `symRight`.
wrap :: String -> String -> String -> String
wrap symLeft symRight str = symLeft ++ str ++ symRight

-- | Remove outer `symLeft`/`symRight` delimiters from `str` if present and not a tuple.
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

-- | Return the string when condition is True, otherwise the empty string.
insertIf :: String -> Bool -> String
insertIf str True = str
insertIf _ False = ""

-- | Apply the function to the `Just` value, or return empty string for `Nothing`.
insertIfJust :: (String -> String) -> Maybe String -> String
insertIfJust f (Just str) = f str
insertIfJust _ Nothing = ""

-- | Operator precedence level for unary and binary IR operators (higher means binds tighter).
opLevel :: Either UnaryOp IRBinaryOp -> Int
opLevel (Left Sqrt) = maxInt    -- these never have to be parenthesized
opLevel (Left Floor) = maxInt
opLevel (Right IRPosPow) = 7
opLevel (Right IRFracPow) = 7
opLevel (Right IRFloatPow) = 7
opLevel (Left Neg) = 6
opLevel (Left Not) = 6
opLevel (Right IRFrac) = 5
opLevel (Right IRDiv) = 5
opLevel (Right IRMult) = 4
opLevel (Right IRMod) = 4
opLevel (Right IRAdd) = 3
opLevel (Right IRSub) = 3
opLevel (Right IREq) = 2
opLevel (Right IRNeq) = 2
opLevel (Right IRLess) = 2
opLevel (Right IRGreater) = 2
opLevel (Right IRLessEq) = 2
opLevel (Right IRGreaterEq) = 2
opLevel (Right IRDivides) = 2
opLevel (Right IRAnd) = 1
opLevel (Right IROr) = 0

-- | Two-space indentation string used by code generation.
tab :: String
tab = "  "
