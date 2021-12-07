{-# OPTIONS_GHC -Wall #-}
module Perun00 where

type Graph  = [[Int]]
data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                  deriving (Show, Eq)
type System = [(String,Recur)] 
data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)

--різне----------------------------------------------

delFirst :: String -> Char -> String
delFirst [] _ = ""
delFirst (a:b) res
    | a==res = b
    | otherwise = [a] ++ delFirst b res

sortChar :: [Char] -> [Char]
sortChar [] = []
sortChar (p:xs) = (sortChar l) ++ [p] ++ (sortChar g)
        where l  = filter (< p) xs
              g = filter (>= p) xs

compareBags :: ([Char],[Char]) -> [Char]
compareBags (xs,[]) = xs
compareBags ([],xs) = xs
compareBags (xs,ys) 
    | head xs == head ys = (head xs):(compareBags (tail xs, tail ys))
    | head xs > head ys = (head ys):(compareBags (xs, tail ys))
    | otherwise = (head xs):(compareBags (tail xs, ys))

delRep :: [Int] ->[Int]
delRep xs 
    | null xs = []
    | otherwise = head xs : delRep (filter ((head xs) /=) xs)

entersNum ::  Int -> [Int]-> Int
entersNum x xs 
    | null xs = 0 
    | x == (head xs) = 1 + entersNum x (tail xs)
    | otherwise = 0 + entersNum x (tail xs)

-- Задача 1 -----------------------------------------
group :: Eq a => [a] -> [[a]]
group []     = []
group (x:xs) = (x : pref) : group suff
    where (pref, suff) = span (x ==) xs
   
-- Задача 2 -----------------------------------------
bagSubbag :: String -> String -> Bool
bagSubbag [] _ = True
bagSubbag _ [] = False
bagSubbag (x:xs) w
    | elem x w = bagSubbag xs (delFirst w x)
    | otherwise = False

-- Задача 3 -----------------------------------------
bagUnion :: String -> String -> String
bagUnion a b = compareBags (sortChar a, sortChar b)

-- Задача 4 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency xs
    | null xs = []
    | otherwise = [(y, entersNum y xs)|y<- delRep xs]

--графи--------------------------------------------

allDiff :: [Int] -> Bool
allDiff [] = True
allDiff (x:xs) = (notElem x xs) && allDiff xs 

isGraph :: Graph -> Bool 
isGraph [] = True
isGraph (g:gr) = (allDiff g) && isGraph gr 

edges::Graph->[(Int,Int)]
edges g=[(x,y)|x<-nodes g,y<-g!!x]

nodes::Graph->[Int]
nodes g=[0..((length g)-1)]

haveWay:: Graph->Int->Int->Bool
haveWay gr x y = not((null (allWays x y (edges gr)))&&(x/=y))

allWays :: Int -> Int -> [(Int, Int)] -> [[Int]] 
allWays a b egs 
    | a == b = [[a]]
    | otherwise = [ a:path | edge <- egs, a == (fst edge), path <- (allWays (snd edge) b (endEdge egs edge))]

endEdge:: [(Int,Int)]->(Int,Int)-> [(Int,Int)]
endEdge egs edge = [e | e <- egs, e /= edge]    

isConnecting :: Graph -> Bool 
isConnecting gr 
    | (isGraph gr) = and [haveWay gr x y|x <- xs, y<-xs]         
    | otherwise = False
        where xs = nodes gr

shortWay :: Graph -> Int -> Int -> [Int]
shortWay gr a b 
    | res==[] = [] 
    | otherwise = (res!!0)
        where res = getNext gr b [[a]]

getNext :: Graph -> Int -> [[Int]] -> [[Int]]
getNext _ _ [] = []
getNext gr b xs 
    | length res > 0 = res 
    |otherwise = getNext gr b ( stepW gr xs)
        where res = [n | n<-xs, n!!(length n-1)==b]

stepW :: Graph -> [[Int]] -> [[Int]]
stepW _ [] = []
stepW gr (w:ws) = [w++[x] | x<-adj, not (elem x w)] ++ stepW gr ws
    where adj = gr!!(w!!(length w - 1))

searchComp :: Graph -> [Int]->[[Int]]->[[Int]] 
searchComp gr nod res 
    | null nod = tail res
    | otherwise = searchComp gr [ y | y<-nod, not( haveWay gr (head nod) y)] (res ++ [[y| y<-nod, haveWay gr (head nod) y]])

-- Задача 5 -----------------------------------------
components :: Graph -> [[Int]] 
components gr
    | isConnecting gr = [nodes gr] 
    | otherwise = searchComp gr (nodes gr) [[]]

-- Задача  6 -----------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr v = maximum [length (shortWay gr v x) - 1 | x<-nodes gr]

-- Задача 7 -----------------------------------------
findDiameter :: Graph -> Int 
findDiameter [] = 0
findDiameter g = maximum [eccentricity g n | n <- [0..(length g - 1)]]

findRadius :: Graph -> Int 
findRadius [] = 0
findRadius g =  minimum [eccentricity g n | n <- [0..(length g - 1)]]

-- Задача 8 -----------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = [x | x<-[0..(length gr - 1)], (eccentricity gr x) == findRadius gr]

--дерева--------------------------------------------

deleteNode :: (Ord a) => BinTreeM a -> BinTreeM a
deleteNode (NodeM _ _ EmptyM  r) = r
deleteNode (NodeM _ _ l  EmptyM) = l
deleteNode (NodeM _ n l r) = NodeM (lastElemL r) n l r

lastElemL :: (Ord a) => BinTreeM a -> a
lastElemL (NodeM v _ EmptyM _) = v
lastElemL (NodeM _ _ t1 _) = lastElemL t1

treeList :: (Ord a) => BinTreeM a -> [a]
treeList EmptyM = []
treeList (NodeM e k tl tr) = treeList tl ++ [e | _ <- [1..k]] ++ treeList tr

--- Задача 9 ----------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM n _ lt rt) v
    |n == v = True
    |v < n = elemSearch lt v
    |otherwise = elemSearch rt v

-- Задача 10 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM x = NodeM x 1 EmptyM EmptyM
insSearch (NodeM n k lt rt) x
    | x == n  = NodeM n (k+1) lt rt
    | x < n   = NodeM n k (insSearch lt x) rt
    | x > n   = NodeM n k lt (insSearch rt x)
    

-- Задача 11 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _=  EmptyM
delSearch (NodeM h n l r) v
    | v == h = if n>1 then NodeM h (n-1) l r else deleteNode (NodeM h n l r )
    | v < h = NodeM h n (delSearch l v) r
    | otherwise = NodeM h n l (delSearch r v)

-- Задача 12 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = treeList (foldl insSearch EmptyM xs)

--рекурсивні-------------------------------------

findFunc :: System -> String -> Recur
findFunc  [] str = error ("func " ++ str ++ " doesn't exist")
findFunc ((name, f):rest) str  
    | name == str  = f
    | otherwise = findFunc rest str

checkF::System->Bool
checkF [] = True
checkF (x:sys) = not (checkD sys (fst x))  && checkF sys

checkN::System->Bool
checkN [] = True
checkN (x:sys) = isRecur sys (snd x) && checkN sys

checkD::System->String->Bool
checkD [] _ = False
checkD (x:sys) f
    |fst x == f =  True
    |otherwise = checkD sys f

checkRN::System->String->Bool
checkRN [] _ = False
checkRN (x:sys) f = (fst x == f) || checkRN sys f

getRec::System -> String-> Recur
getRec syst s | null syst = error "no func"
                | fst(head syst)==s = snd (head syst)
                | otherwise = getRec (tail syst) s

contRec:: System -> String -> Bool
contRec syst s | null syst = False
                     | fst(head syst)==s = True
                     | otherwise = contRec (tail syst) s


endCond :: ([Int],Int)->Bool
endCond (xs,n) = last(init xs)==n

primStep :: System->Recur->([Int],Int)->([Int],Int)
primStep syst r (xs,n) = (init(init xs)++[(last (init xs))+1]++[eval syst r xs], n)

-- Задача 13 ------------------------------------
isNumbConst :: System -> Recur -> Bool
isNumbConst _ Zero = True
isNumbConst syst (Super Succ [f]) = isNumbConst syst f
isNumbConst syst (Name f) = isNumbConst syst (findFunc syst f)
isNumbConst _ _ = False

-- Задача 14 ------------------------------------
evRank :: System -> Recur -> Int
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = (evRank syst st)-1
evRank syst (Mini b _) = (evRank syst b)-1
evRank syst (Name f) = evRank syst (findFunc syst f )

-- Задача 15 ------------------------------------
isNames :: System -> Bool
isNames syst = checkF syst && checkN (reverse syst)

-- Задача 16 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur _ Zero = True
isRecur _ Succ = True
isRecur _ (Sel _ _)  = True
isRecur sys (Prim a b)  = isRecur sys a && isRecur sys b
isRecur sys (Mini a _)  = isRecur sys a
isRecur sys (Super a b) = isRecur sys a && and [isRecur sys x | x <- b]
isRecur sys (Name str) = checkRN (reverse sys) str

-- Задача 17 ------------------------------------
eval :: System -> Recur -> [Int] -> Int 
eval _ (Zero) _= 0 
eval _ (Succ) xs = (head xs) +1
eval _ (Sel n k) xs 
    | (k<=n)&&(length xs >= n) = xs!!(k-1) 
    |otherwise = 0
eval syst (Super f gn) xs = eval syst f [eval syst g xs|g<-gn]
eval _ (Mini _ _) _ = 0

eval syst (Name nm) xs 
    | contRec syst nm = eval syst (getRec syst nm) xs 
    | otherwise = error "undefined function"

eval syst (Prim r1 r2) xs =  last(fst(until endCond  (primStep syst r2) config))
    where config = ((init xs)++[0]++[(eval syst r1 (init xs))],last xs)

-- Задача 18 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart = undefined


---------------------Тестові дані - Графи -------
gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

---------------------Тестові дані - Рекурсивні функції -------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]