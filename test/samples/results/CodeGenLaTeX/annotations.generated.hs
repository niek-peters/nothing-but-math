{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module TestModule (TestModule.f, TestModule.g, TestModule.abs, TestModule.h, TestModule.id, TestModule.neg, TestModule.i, TestModule.k, TestModule.l) where

import GHC.Num (Natural)
import Data.Ratio ((%), denominator, numerator)

f :: Natural -> Natural -> Integer
f a b
  | otherwise = c
  where
    c = (widen @Integer a) - (widen @Integer b)

g :: Natural -> Natural -> Integer
g a b
  | otherwise = widen @Integer d
  where
    d = a + b

abs :: Integer -> Natural
abs x
  | x < (widen @Integer (0 :: Natural)) = narrow @Natural (-x)
  | otherwise = narrow @Natural x

h :: Natural -> Natural -> Rational
h a b
  | otherwise = (widen @Integer e) % (widen @Integer (6 :: Positive))
  where
    e = TestModule.abs ((TestModule.f a b) * (TestModule.g a b))

id :: Integer -> Integer
id x
  | otherwise = x

neg :: Integer -> Integer
neg x
  | otherwise = -(widen @Integer (TestModule.abs x))

i :: Integer -> Integer
i x
  | otherwise = TestModule.id x

k :: Integer -> Integer
k x
  | otherwise = (-(widen @Integer (2 :: Positive))) * (TestModule.neg y)
  where
    y = TestModule.i x

l :: Integer -> Integer
l x
  | otherwise = x ^ (widen @Natural (2 :: Positive))



-- PRELUDE --

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
    toInteger (Positive n) = n
    quotRem (Positive a) (Positive b) = (Positive q, Positive r)  -- crashes if remainder is 0. This is fine, in that case the user should have specified Natural or Integer instead
        where 
            (q, r) = quotRem a b


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