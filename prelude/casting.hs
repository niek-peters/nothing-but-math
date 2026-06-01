-- RUNTIME NUMBER TYPE CASTING --
-- we define a typeclass with implementations for type narrowing
class Narrow to from where
    narrow :: from -> to

-- first we implement the individual narrowing steps
-- Real -> Rational
instance Narrow Rational Double where 
    narrow r
        | Prelude.abs (r - fromIntegral rounded) < 1e-8 = toRational rounded    -- round floating point to nearest integer if close enough (to alleviate floating-point errors)
        | otherwise = toRational r
        where   rounded = round r :: Integer

-- Rational -> Integer
instance Narrow Integer Rational where
    narrow r 
        | denominator r == 1 = numerator r
        | otherwise  = error $ 
            "RUNTIME ERROR: Invalid cast from Rational to Integer. " 
            ++ show r ++ " is not a whole number."

-- Integer -> Natural
instance Narrow Natural Integer where
    narrow i 
        | i >= 0    = fromInteger i
        | otherwise = error $ 
            "RUNTIME ERROR: Invalid cast from Integer to Natural. " 
            ++ show i ++ " is negative."

-- Natural -> Positive
instance Narrow Positive Natural where
    narrow n
        | n /= 0     = fromIntegral n
        | otherwise = error $
            "RUNTIME ERROR: Invalid cast from Natural to Positive. Value is 0."

-- then we implement shortcuts for direct narrowing from any supertype to any subtype
-- Real -> Integer
instance Narrow Integer Double where narrow r = narrow @Integer $ narrow @Rational r
-- Real -> Natural
instance Narrow Natural Double where narrow r = narrow @Natural $ narrow @Rational r
-- Real -> Positive
instance Narrow Positive Double where narrow r = narrow @Positive $ narrow @Rational r
-- Rational -> Natural
instance Narrow Natural Rational where narrow r = narrow @Natural $ narrow @Integer r
-- Rational -> Positive
instance Narrow Positive Rational where narrow r = narrow @Positive $ narrow @Integer r
-- Integer -> Positive
instance Narrow Positive Integer where narrow i = narrow @Positive $ narrow @Natural i


-- similarly, we define a typeclass with implementations for type widening
class Widen to from where
    widen :: from -> to

-- Positive -> Natural
instance Widen Natural Positive where widen = fromIntegral
-- Natural -> Integer
instance Widen Integer Natural where widen = fromIntegral
-- Integer -> Rational
instance Widen Rational Integer where widen = fromIntegral
-- Rational -> Real
instance Widen Double Rational where widen = fromRational

-- then we implement shortcuts for direct widening from any subtype to any supertype
-- Positive -> Integer
instance Widen Integer Positive where widen = fromIntegral
-- Positive -> Rational
instance Widen Rational Positive where widen = fromIntegral
-- Positive -> Real
instance Widen Double Positive where widen = fromIntegral
-- Natural -> Rational
instance Widen Rational Natural where widen = fromIntegral
-- Natural -> Real
instance Widen Double Natural where widen = fromIntegral
-- Integer -> Real
instance Widen Double Integer where widen = fromIntegral