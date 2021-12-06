{-# OPTIONS_GHC -Wall #-}
module Perun01 where

-- ������ 1 -----------------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*factorial(n-1)

-- ������ 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if null xs then ys else if null ys then xs else (head xs + head ys):listSum (tail xs) (tail ys)

-- ������ 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven [] = []
oddEven [xs] = [xs]
oddEven (x : y : xs) = y : x : oddEven xs

-- ������ 4 -----------------------------------------
position    ::  Int -> [Int] -> Int
position _ [] = -1
position n xs = if notElem n xs then -1 else if n == head xs then 0 else 1 + (position n (tail xs))
                     
-- ������ 5 -----------------------------------------
set :: [Int] -> [Int] 
set [] = []
set [xs] = [xs]
set (x:xs) = if notElem x xs then x:set xs else set xs

-- ������ 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (xs ++ ys)

-- ������ 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = set ([x | x <-xs, y <- ys, x==y])

-- ������ 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1..]]