module CodeGen (module CodeGen) where
        
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List (intercalate)
import IR (IRBinaryOp (..))

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

binaryOpLevel :: IRBinaryOp -> Int
binaryOpLevel IRPow = 4
binaryOpLevel IRExp = 4
binaryOpLevel IRFrac = 3
binaryOpLevel IRDiv = 3
binaryOpLevel IRMult = 2
binaryOpLevel IRMod = 2
binaryOpLevel IRAdd = 1
binaryOpLevel IRSub = 1
binaryOpLevel IREq = 0
binaryOpLevel IRNeq = 0
binaryOpLevel IRLess = 0
binaryOpLevel IRGreater = 0
binaryOpLevel IRLessEq = 0
binaryOpLevel IRGreaterEq = 0
binaryOpLevel IRDivides = 0



tab = "  "
