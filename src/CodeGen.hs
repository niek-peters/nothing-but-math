module CodeGen (module CodeGen) where
        
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List (intercalate)
import IR (IRBinaryOp (..))
import GHC.Base (maxInt)
import Token (UnaryOp (..))

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

insertIf :: String -> Bool -> String
insertIf str True = str
insertIf _ False = ""

insertIfJust :: (String -> String) -> Maybe String -> String
insertIfJust f (Just str) = f str
insertIfJust _ Nothing = ""

opLevel :: Either UnaryOp IRBinaryOp -> Int
opLevel (Left Sqrt) = maxInt    -- these never have to be parenthesized
opLevel (Left Floor) = maxInt
opLevel (Right IRPow) = 7
opLevel (Right IRExp) = 7
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


tab = "  "
