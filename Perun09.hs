{-# OPTIONS_GHC -Wall #-}
module Perun09 where

import Text.ParserCombinators.Parsec
import Data.Char (isDigit)
import Data.List

data Term   =  Nmb Int         -- десяткове число без знаку
            | Var String       -- змінна, довільний ідентифікатор
            | App Term Term    -- операція застосування
            | Abs String Term  --  операція абстракції
           deriving (Show, Eq) 
type Contex = [(String,Term)]

-- Задача 1.a -----------------------------------------
addVar :: String -> [String] -> [String] 
addVar s xs = if s `elem` xs then xs else  s:xs

-- Задача 1.b ----------------------------------------- 
delVar :: String -> [String] -> [String]
delVar _ []                 = []
delVar x (y:ys) | x == y  = delVar x ys
                | otherwise = y : delVar x ys

-- Задача 1.c -----------------------------------------
unionV :: [String] -> [String] -> [String]
unionV xs ys = nub $ xs ++ ys
    
-- Задача 1.d ----------------------------------------- 
freeVars :: Term -> [String]
freeVars t = fvH t []
                where fvH (Nmb _) vs = vs
                      fvH (Var v) vs = addVar v vs
                      fvH (Abs x z) vs = delVar x (fvH z vs)
                      fvH (App t1 t2) vs = unionV (fvH t1 vs) (fvH t2 vs)


-- Задача 2.a -----------------------------------------
deleteSyn :: String -> Contex -> Contex
deleteSyn = undefined 

-- Задача 2.b -----------------------------------------
iswfTerm :: Term -> Contex -> Bool
iswfTerm = undefined 

-- Задача 2.c -----------------------------------------
iswfContex :: Contex -> Bool
iswfContex = undefined 

-- Задача 3.a -----------------------------------------
isNumber :: Term -> Bool
isNumber = undefined

-- Задача 3.b -----------------------------------------
inNumber :: Term -> Term
inNumber = undefined 

-- Задача 3.c -----------------------------------------
compress ::  Term -> Term
compress = undefined

-- Задача 4 -----------------------------------------
reduce :: Term -> String -> Term -> Term 
reduce = undefined

-- Задача 5 -----------------------------------------
evalStep :: Term -> Contex -> Maybe Term   
evalStep = undefined

-- Задача 6 -----------------------------------------
eval :: Int -> Term -> Contex -> Maybe Term 
eval = undefined 

-- Задача 7 -----------------------------------------
parseTerm :: String -> Maybe Term 
parseTerm = undefined     

--------------------------------------------------------
-- integerTerm - з числа в вираз
integerTerm :: Int ->  Term
integerTerm n  = (Abs "s" (Abs "z" (buildTerm n))) 
  where buildTerm 0 = Var "z" 
        buildTerm j = (App (Var "s") (buildTerm (j-1)))  

--  New Name -- якщо імя dddname, де ddd-цифри і n-буква, то початкове імя - name 
-- якщо змінна ccc, то її нові імена 0ccc,...,9ccc,09ccc,...
-- цифри на початку - це створення нового імені (problem name capture)
newVar :: [String] -> String -> String
newVar fvs nm = (until (\n -> notElem n fvs) next) (next nm)   -- flip elem fvs
  where next n@(c:_)| c=='9'    = '0':n 
        next (c:cx) | isDigit c = (succ c):cx 
        next n      = '0':n

--------------------------------------------------------
-- Тестові приклади
term0, term0a, term1, term1a, term1b, term1c :: Term
term0 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z")))) 
term0a = Abs "z" (App (Var "s") (App (Var "s") (Var "z")))
term1 = Abs "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
term1a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
term1b = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
term1c = Abs "y" (App (Var "x") (Var "y"))

term2, term2a, termAnd, termTest :: Term
term2 = App (App (Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
                 (Abs "x" (Abs "y" (Var "x")))
            ) 
            (Abs "x" (Abs "y" (Var "x")))
term2a = App (Var "x") (App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Var "z")))
termAnd = Abs "x" (Abs "y" (App (App  (Var "x") (Var "y")) (Var "false")))
termTest = Abs "x" (Abs "x" (Abs "y" (Var "y")))

cont1 :: Contex
cont1 = [("true",Abs "x" (Abs "y" (Var "x")))
        ,("false",Abs "x" (Abs "y" (Var "y")))
        ,("test",Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n")))))
        ,("iszero",Abs "n" (App (App (Var "n") (Abs "x" (Var "false"))) (Var "true")))
        ,("plus",Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))))
        ,("mult",Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))))
        ,("pred",Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "x" (Var "x"))))))
        ,("fixM",Abs "f" (App (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y")))))))
        ,("sumR",Abs "r" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 0)) (App (App (Var "plus") (Var "n")) (App (Var "r") (App (Var "pred") (Var "n")))))))
        ,("factR",Abs "fact" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 1)) (App (App (Var "mult") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n")))))))
        ,("sum",App (Var "fixM") (Var "sumR"))
        ,("factor",App (Var "fixM") (Var "factR"))
        ]

termS2 :: String 
termS2 = "(\\f g x. f x (g x))   (\\x y .x) (\\x y .x)"