import Data.Ratio ((%), denominator, numerator)
import GHC.Natural (Natural)

a :: Rational
a  = (5 % 2)

b :: Integer
b   | denom == 1 = num
    | otherwise = error "GG"
    where   num = numerator a
            denom = denominator a


c = fromIntegral :: Integer -> Rational

d :: Integer
d = 1

e :: Rational
e = fromIntegral d



-- POSITIVE NUMBER TYPE --
newtype Positive = Positive Integer
    deriving (Eq, Ord)

-- implement showing without wrapper type
instance Show Positive where
  show (Positive n) = show n

-- implement arithmetic operations
instance Num Positive where
  (Positive a) + (Positive b) = Positive $ a + b
  (Positive a) - (Positive b)   | res > 0 = Positive res
                                | otherwise = error $ "Positive: subtraction resulted in arithmetic underflow: " ++ show res
    where res = a - b
  (Positive a) * (Positive b) = Positive $ a * b
  abs n = n
  signum _ = Positive 1
  fromInteger n | n > 0 = Positive n
                | otherwise = error $ "Positive: fromInteger used on non-positive value: " ++ show n

-- needed for Integral
instance Enum Positive where
  toEnum n = fromInteger (toInteger n)
  fromEnum (Positive n) = fromEnum n

-- needed for Integral
instance Real Positive where
  toRational (Positive n) = toRational n

-- needed for upcasting Positive with fromIntegral
-- also implements mod operation
instance Integral Positive where
  toInteger (Positive n ) = n
  quotRem (Positive a) (Positive b) = (Positive q, Positive r)  -- crashes if remainder is 0. This is fine, in that case the user should have specified Natural or Integer instead
      where 
        (q, r) = quotRem a b