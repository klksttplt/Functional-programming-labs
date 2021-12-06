{-# OPTIONS_GHC -Wall #-}
module Perun02 where

-- ������ 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs
  
-- ������ 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]

-- ������ 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr xs ys  = foldr (:) ys xs

-- ������ 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
insert :: [Integer] -> Integer -> [Integer]
insert xs v = [x|x <- xs, x < v] ++ [v] ++ [x | x <- xs, x >= v]
sortInsert xs = foldl (insert) [] xs

-- ������ 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys


-- ������ 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sum [(fromInteger(m)^i/fromInteger(factorial i))|i<-[1..n]]


-- ������ 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- ������ 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [x*x|x<-[1..]]

-- ������ 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys= [i|i <- [0.. length ys], xs == take (length xs)(drop i ys)]

