{-# OPTIONS_GHC -Wall #-}
module Perun03 where

type Graph  = [[Int]]

nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

edges :: Graph -> [(Int,Int)]
edges e = [(x,y) | x<-nodes e, y <- e!!x] 

edgeIn::Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)


-- ������ 1 ------------------------------------

allDiff :: [Int] -> Bool
allDiff [] = True
allDiff (x:xs) = (notElem x xs) && allDiff xs 

isGraph :: Graph -> Bool 
isGraph [] = True
isGraph (g:gr) = (allDiff g) && isGraph gr 

-- ������ 2 ------------------------------------

haveEdge :: Int -> Int -> Graph -> Bool
haveEdge x y gr = edgeIn gr (x, y) || edgeIn gr (y, x)

isEdge:: Graph -> [Bool]
isEdge gr = [haveEdge a b gr | a <- nodes gr, b <- nodes gr, a /= b]

isTournament :: Graph -> Bool 
isTournament gr = head (isEdge gr) && head (isEdge (tail gr));

-- ������ 3 ------------------------------------

wayExists :: Graph -> (Int,Int) -> (Int,Int) -> Bool
wayExists g (x,_) (_,y) = (x,y) `elem` edges g

isTransitive :: Graph -> Bool 
isTransitive g = and [wayExists g (a,a1) (b2,b) | (a,a1) <- edges g, (b2,b) <- edges g, a1 == b2]

-- ������ 4 ------------------------------------

buildTransitive :: Graph -> Graph 
buildTransitive = undefined
-- ������ 5 ------------------------------------

longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay = undefined

-- ������ 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay = undefined

-- ������ 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic = undefined

-- ������ 8 ------------------------------------
topolSort :: Graph -> Maybe [Int] 
topolSort = undefined

-- ������ 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort = undefined

--------------------------------------------------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]