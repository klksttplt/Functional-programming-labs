{-# OPTIONS_GHC -Wall #-}
module Perun07 where 

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne] 
                 deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
coef :: Rational -> PolinomOne -> PolinomOne 
coef c p0  = map (\x -> (fst x, c*snd x)) p0

-- Задача 1.b -----------------------------------------
add :: PolinomOne -> PolinomOne -> PolinomOne
add = undefined

-- Задача 1.c -----------------------------------------
unify::PolinomOne -> PolinomOne
unify = undefined

-- Задача 2.a -----------------------------------------
findFree :: [PolinomOne] -> [Int]
findFree = undefined             
                     
-- Задача 2.b -----------------------------------------
iswfCommon ::  [PolinomOne]  -> Bool 
iswfCommon = undefined

-- Задача 3.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple le = if length (filter (\x -> length x == 1) le) == length le then True else False

-- Задача 3.b -----------------------------------------
sshelp :: Linear -> Maybe [PolinomOne]
sshelp le = if (length (filter (\x -> x!!0 == 0) le) == length le) then Just[] else Nothing

solveSimple :: Linear -> Maybe [PolinomOne] 
solveSimple le = if(isSimple le) then sshelp le else Nothing

-- Задача 4.a -----------------------------------------
frhelp :: Linear -> Int -> Int
frhelp le i = if ((head $ head le) /= 0) then i else frhelp (tail le) (i+1)

findRow :: Linear -> Maybe Int
findRow le = if ( length ( filter (\x -> (head x /= 0)) le ) ) /= 0 then Just (frhelp le 1) else Nothing

-- Задача 4.b -----------------------------------------
erhelp :: Int -> Int -> [a] -> [a]
erhelp a b xs = xs1 ++ [xs !! b] ++ xs2 ++ [xs !! a] ++ xs3
    where   xs1 = take a xs;
            xs2 = drop (succ a) (take b xs);
            xs3 = drop (succ b) xs

exchangeRow :: [a] -> Int -> [a]
exchangeRow le k = erhelp 0 (k-1) le

-- Задача 5.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep = undefined

-- Задача 5.b -----------------------------------------
reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep = undefined

-- Задача 6 -----------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne] 
gauss = undefined

-- Задача 7.a -----------------------------------------
testEquation :: [PolinomOne] -> Row -> Bool 
testEquation = undefined

-- Задача 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool 
testLinear = undefined

-- Задача 8 -----------------------------------------
solving :: Linear -> Solution  
solving = undefined

-------------------------------------------------------
pol0, pol1, pol2, pol3, pol4 :: PolinomOne 
pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
pol2 = [(0,15), (4,3),(5,1)]
pol3 = [(0,-10), (2,7), (4,-3)]
pol4 = [(0,-26/15), (2,3), (3,5/7)]

test0, test1, test2, test3, test3a, test4 :: Linear
test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

res3, res4 :: [PolinomOne]
res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

sol1,sol2,sol3,sol4 :: Solution
sol1 = Empty 
sol2 = Empty 
sol3 = Many res3 
sol4 = One [62/15, -17/15, -4/3] 


