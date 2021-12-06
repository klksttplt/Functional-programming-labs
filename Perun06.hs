{-# OPTIONS_GHC -Wall #-}
module Perun06 where

import Data.List

newtype Poly a = P [a]


-- ������ 1 -----------------------------------------
x :: Num a => Poly a
x = P[0,1]

-- ������ 2 ----------------------------------------
zero :: (Eq a, Num a) => [a] -> [a]
zero = reverse.dropWhile(==0).reverse

instance (Num a, Eq a) => Eq (Poly a) where
     P a == P b = zero a == zero b
 
-- ������ 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show a = intercalate " + " (format 0 a)
      where format :: (Num a, Eq a, Show a) => Int -> Poly a -> [String]
            format _ (P []) = []
            format 0 (P (z:[])) = [show z]
            format 1 (P (z:[])) = [show z ++ "x"]
            format y (P (z:[]))
              | z == 0 = []
              | otherwise = [(show z) ++ "x^" ++ (show y)]
            format y (P (z:zs))
              | z == 0    = format (y+1) (P zs)
              | otherwise = format (y+1) (P zs) ++ format y (P [z])

-- ������ 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ add a b
  where
    add [] y = y
    add z [] = z
    add (z:zs) (y:ys) = z + y : add zs ys

-- ������ 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P []) (P _) = P [0]
times (P (d:ds)) (P ys) = (P (map (* d) ys)) + (times (P ds) (P ([0] ++ ys)))

-- ������ 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P []) = P []
    negate (P b) = P (map negate b)
    fromInteger i = P [fromInteger i]
    -- �������� �������� �� ����
    abs    = undefined
    signum = undefined

-- ������ 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P p) v = sum.zipWith (*) p $ iterate (* v) 1

-- ������ 8 -----------------------------------------
class Num a => Differentiable a where
    derive  :: a -> a
    nderive :: Int -> a -> a
    nderive n z = iterate derive z !! n

-- ������ 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    derive (P b) = P (zipWith (*) (tail b) (map (fromIntegral) [1..length b-1]))
