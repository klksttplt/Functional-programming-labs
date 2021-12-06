{-# OPTIONS_GHC -Wall #-}
module Perun10 where

import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Char(isDigit)

data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- Змінна
         | Const Value     -- константа
         | Op Exp Bop Exp  -- Операція
                 deriving (Show, Eq)
-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | And | Or
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Read String 
          | Write Exp
          | Incr String
          | If Exp Stmt 
          | While Exp Stmt       
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]        
          deriving (Show, Eq)
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = ([String], [(String,Value)], [String])

type VarEnv  = [(String,Type)]

--send help------
fsth :: (a, b, c) -> a
fsth (x, _, _) = x
sndh :: (a, b, c) -> b
sndh (_, x, _) = x
thrdh :: (a, b, c) -> c
thrdh (_, _, x) = x

isNum :: String -> Bool
isNum ""  = False
isNum "." = False
isNum xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

stringVal :: String -> Value
stringVal s 
  | s == "True" = (B True)
  |otherwise = (B False)
stringType :: String -> Type
stringType s 
  | s == "True" && s == "False" = Bt
  | otherwise = It

-- Задача 1.a -----------------------------------------
getValue::  StateW -> String -> Value
getValue st id = fromJust (lookup id (sndh st))

-- Задача 1.b -----------------------------------------
updValue :: StateW -> String -> Value -> StateW
updValue st id v = ((fsth st), map (\x -> if fst x == id then (id, v) else x) (sndh st), (thrdh st))
  
-- Задача 2 ----------------------------------------- 
readValue :: StateW -> Type -> (StateW,Value)
readValue st (It)
  | null (fsth st) = (st, (I 0))
  | isNum (head (fsth st)) = ((tail ( (fsth st)), (sndh st), (thrdh st)), (I (read (head (fsth st)))))
  | otherwise = (st, (I 0))
readValue st (Bt)
  | null (fsth st) = (st, (B False))
  | (head (fsth st)) == "True" || (head (fsth st)) == "False" = ((tail  ((fsth st)), (sndh st), (thrdh st)), (stringVal (head (fsth st))))
  | otherwise = (st, (B False))

-- Задача 3 -----------------------------------------

writeValueH :: Value -> String
writeValueH v = filter (not . (`elem` "IB ")) (show v)

writeValue :: StateW -> Value -> StateW 
writeValue st v = ((fsth st), (sndh st), ((thrdh st) ++ [writeValueH v]))
  
-- Задача 4.a ----------------------------------------- 

opBool :: Value -> Value -> (Bool->Bool->Bool) -> Value
opBool (B v1) (B v2) f = (B (f v1 v2))
opBool _ _ _ = error "not boolean"

opInt :: Value -> Value -> (Int->Int->Int) -> Value
opInt (I v1) (I v2) f = (I (f v1 v2))
opInt _ _ _ = error "not integer"

opBoolInt :: Value -> Value -> (Int->Int->Bool) -> Value
opBoolInt (I v1) (I v2) f = (B (f v1 v2))
opBoolInt _ _ _ = error "not integer"

evExp :: StateW -> Exp -> Value
evExp _ (Const v) = v
evExp st (Var s) | null s = (B False)
                 | otherwise = getValue st s
evExp st (Op e1 Plus e2) = opInt (evExp st e1) (evExp st e2) (+)
evExp st (Op e1 Minus e2) = opInt (evExp st e1) (evExp st e2) (-)
evExp st (Op e1 Times e2) = opInt (evExp st e1) (evExp st e2) (*)
evExp st (Op e1 Div e2) = opInt (evExp st e1) (evExp st e2) (div)
evExp st (Op e1 Gt e2) = opBoolInt (evExp st e1) (evExp st e2) (>)
evExp st (Op e1 Ge e2) = opBoolInt (evExp st e1) (evExp st e2) (>=)
evExp st (Op e1 Lt e2) = opBoolInt (evExp st e1) (evExp st e2) (<)
evExp st (Op e1 Le e2) = opBoolInt (evExp st e1) (evExp st e2) (<=)
evExp st (Op e1 Eql e2) = opBoolInt (evExp st e1) (evExp st e2) (==)
evExp st (Op e1 And e2) = opBool (evExp st e1) (evExp st e2) (&&)
evExp st (Op e1 Or e2) = opBool (evExp st e1) (evExp st e2) (||)

-- Задача 4.b -----------------------------------------
evStmt :: StateW -> Stmt -> StateW 
evStmt = undefined

-- Задача 4.c -----------------------------------------
evProgram :: Program -> [String] -> [String]
evProgram = undefined

---- Перевірка контекстних умов -----------------------
-- Задача 5.a -----------------------------------------
iswfOp :: Bop -> [Type] -> Maybe Type 
iswfOp bop xs
  | (bop == Plus || bop == Minus || bop == Times || bop == Div) &&
      length xs == 2 && ((head xs) == It && (last xs) == It) = Just It
  | (bop == Eql || bop == And || bop == Or) &&
      length xs == 2 &&
      (((head xs) == It && (last xs) == It) ||
       ((head xs) == Bt && (last xs) == Bt)) = Just Bt
  | length xs == 2 && ((head xs) == It && (last xs) == It) = Just Bt
  | otherwise = Nothing

-- Задача 5.b -----------------------------------------
iswfExp :: Exp -> VarEnv -> Maybe Type 
iswfExp (Const (B _)) _ = Just Bt
iswfExp (Const (I _)) _ = Just It
iswfExp (Var x) var = lookup x var 
iswfExp (Op xs bop ys) var
  | (iswfExp xs var) == Nothing || (iswfExp ys var) == Nothing = Nothing
  | otherwise = iswfOp bop [fromJust (iswfExp xs var), fromJust (iswfExp ys var)]

-- Задача 5.c -----------------------------------------
iswfStmt :: Stmt -> VarEnv -> Bool 
iswfStmt = undefined

iswfProgram :: Program -> Bool 
iswfProgram st = iswfStmt st []

---- Синтаксичний аналіз -------
iden :: Parser String
iden = try( do {nm <- identif;
                if (any(nm==) ["int","bool","read","write","if","while","for","true","false"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm 
               } ) 

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop   
mulOp = (oper "*" Times) <|> (oper "/" Div)

disOp :: Parser Bop   
disOp = (oper "&" And)

conOp :: Parser Bop   
conOp = (oper "|" Or)

--розпізнавати ВСІ порожні символи в кінці
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

--   :type Op -----> Exp -> Bop -> Exp -> Exp 
--   :type flip Op -------> Bop -> Exp -> Exp -> Exp         
expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum)) 

typev :: Parser Type 
typev = do {keyword "int"; return It}
        <|> do {keyword "bool"; return Bt} 

-- Задача 6.a -----------------------------------------
identif :: Parser String
identif = do {l <- letter; ltrs <- many (digit <|> letter); return (l:ltrs)}

number :: Parser Int
number = read <$> (many1 digit)
 
addOp :: Parser Bop  
addOp = oper "+" Plus <|> oper "-" Minus

relOp :: Parser Bop  
relOp = try (oper "<=" Le) <|> try (oper ">=" Ge) <|> try (oper "==" Eql)
     <|> try (oper "<" Lt) <|> try (oper ">" Gt)

-------------------------------------------------------
factor :: Parser Exp
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {keyword "true"; return (Const (B True))}
     <|> do {keyword "false"; return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

-- Задача 6.b -----------------------------------------
term :: Parser Exp     
term = chainl1 factor $ expOp mulOp

relat :: Parser Exp
relat = chainl1 term $ expOp addOp

conj :: Parser Exp
conj = chainl1 relat $ expOp relOp

disj :: Parser Exp
disj = chainl1 conj $ expOp conOp

expr :: Parser Exp
expr = chainl1 disj $ expOp disOp
------------------------------------------------------
stmt :: Parser Stmt 
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {keyword "read"; inSt}
       <|> do {keyword "write"; outSt}
       <|> do {var <- lexem iden; assignSt var}
       <|> blockSt
       <?> "statement"

-- Задача 6.c -----------------------------------------
forSt :: Parser Stmt  
forSt   = undefined

whileSt :: Parser Stmt               
whileSt = undefined 
              
ifSt :: Parser Stmt              
ifSt    = undefined  

inSt :: Parser Stmt              
inSt    = undefined  

outSt :: Parser Stmt              
outSt    = undefined  

assignSt :: String -> Parser Stmt 
assignSt = undefined

blockSt :: Parser Stmt
blockSt = undefined
               
---------------------------------------------	
-- Головні функції
---------------------------------------------				
program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseL :: String -> Either ParseError Program
parseL s = parse program "" s

-- Програми -------------------------------------------
squareRoot :: Program
squareRoot = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "a") Ge (Const(I 0)))
                        (Block [("c", Bt)] 
                               [Assign "c" (Const (B True)),
                                While (Var "c")
                                 (Block []
                                   [(Incr "b"), 
                                    Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                               ]
                        )
                   , Write (Op (Var "b") Minus (Const (I 1)))
                   ]

squareRootS :: String
squareRootS =
   "{int a, b; \
   \ read a; b := 0; \
   \ if (a>= 0)\
   \    {bool c; c:=true; while(c) {b++; c:= a >= b*b}\
   \    };\
   \  write (b-1)\
   \ }"

fibonacci :: Program
fibonacci = 
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Ge (Const(I 0))) 
               (Block [("f0",It), ("f1",It), ("c",It)]
                     [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)), Assign "out" (Const (I 1)),
                      If (Op (Var "in") Gt (Const (I 1)))
                         (For (Assign "c" (Const (I 1)))
                             (Op (Var "c") Lt (Var "in")) 
                             (Incr "c")
                             (Block []
                                    [Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                    , Assign "f0" (Var "f1")
                                    , Assign "f1" (Var "out")
                                    ]
                              )
                         )
                     ])
          , Write (Var "out")
          ]

fibonacciS :: String
fibonacciS = 
 " {int in, out; read in; out := 0; \n\
   \if (in>=0){int f0, f1,c; \n\
   \           f0 := 1; f1 := 1; out := 1; \n\
   \           if(in>1) \n \
   \              for (c := 1; c < in; c++) {\n\
   \                   out := f0 + f1; f0 := f1; f1 := out\n\
   \              }\n\
   \          }; \n\
   \write out \n\
  \}"
