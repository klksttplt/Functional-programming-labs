{-# OPTIONS_GHC -Wall #-}
module Perun08 where

data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- ������� 2-3-������!!!
                   deriving (Eq, Show)


-- ������ 1 -----------------------------------------

isEmpty :: BinTree a -> Bool
isEmpty EmptyB = True
isEmpty _ = False

getNode::BinTree a -> a
getNode (Node a _ _) = a
getNode EmptyB = error "empty - no node"

childLeft::BinTree a -> BinTree a
childLeft (Node _ x _) = x
childLeft EmptyB = error "empty - no child"

childRight::BinTree a -> BinTree a
childRight (Node _ _ x) = x
childRight EmptyB = error "empty - no child"

isSearch :: (Ord a) => BinTree a -> Bool
isSearch a
    |isEmpty a = True
    |isEmpty (childLeft a) && isEmpty (childRight a) = True
    |isEmpty (childLeft a) = getNode a < getNode (childRight a) && isSearch (childLeft a) && isSearch (childRight a)
    |isEmpty (childRight a) = getNode a > getNode (childLeft a) && isSearch (childLeft a) && isSearch (childRight a)
    |otherwise = getNode a > getNode (childLeft a) && getNode a < getNode (childRight a) && isSearch (childLeft a) && isSearch (childRight a)

-- ������ 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch a x 
    |isEmpty a = False
    |getNode a == x = True
    |getNode a < x = elemSearch (childRight a) x
    |otherwise = elemSearch (childLeft a) x

-- ������ 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch EmptyB v = Node v EmptyB EmptyB
insSearch (Node p left right) v
        | v == p = (Node p left right)
        | v > p = (Node p left (insSearch right v))
        | otherwise = (Node p (insSearch left v) right)


-- ������ 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a 
delSearch EmptyB v = Node v EmptyB EmptyB
delSearch (Node p left right) v
        | v == p = merge left right
        | v > p = (Node p left (delSearch right v))
        | otherwise = (Node p (delSearch left v) right) where
                merge tr EmptyB = tr
                merge EmptyB tr = tr
                merge (Node h leftn rightn) tr = Node h leftn (merge rightn tr)

-- ������ 5 -----------------------------------------
listB :: (Ord a) => BinTree a -> [a]
listB EmptyB = []
listB (Node p left right) = listB left ++ p : listB right

sortList :: (Ord a) => [a] -> [a]
sortList xs = listB (foldl insSearch EmptyB xs)

-- ������ 6-----------------------------------------
maxT :: (Ord a) => Tree23 a -> a
maxT Empty23 = error "empty 23"
maxT (Leaf n) = n
maxT (Node2 _ n Empty23) = n
maxT (Node2 _ _ tr) = maxT tr
maxT (Node3 _ _ _ n Empty23) = n
maxT (Node3 _ _ _ _ tr) = maxT tr

minT :: (Ord a) => Tree23 a -> a
minT Empty23 = error ("empty 23")
minT (Leaf m) = m
minT (Node2 Empty23 m _) = m
minT (Node2 tl _ _) = minT tl
minT (Node3 Empty23 m _ _ _) = m
minT (Node3 tl _ _ _ _) = minT tl

isTree23  :: (Ord a) => Tree23 a -> Bool
isTree23 Empty23 = True
isTree23 (Leaf a) = a==a
isTree23 (Node2 tl a tr) = a >= maxT tl && a == minT tr
isTree23 (Node3 tl a tm b tr) = a >= maxT tl && a == minT tm && b >= maxT tm && b == minT tr

-- ������ 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 (Empty23) _ = False
elemTree23 (Leaf a) p = a==p

elemTree23 (Node2 (a) b (c)) p
 | p < b = elemTree23 (a) p
 | p > b = elemTree23 (c) p
 | p == b = True
 | otherwise = False

elemTree23 (Node3 (a) b (c) d (e)) p
 | p < b = elemTree23 (a) p
 | p > b && p < d = elemTree23 (c) p
 | p > d = elemTree23 (e) p
 | p == b = True
 | p == d = True
 | otherwise = False

-- ������ 8-----------------------------------------
list23 :: (Ord a) => Tree23 a -> [a]
list23 Empty23 = []
list23 (Leaf l) = [l]
list23 (Node2 s1 _ s2) = list23 s1 ++ list23 s2
list23 (Node3 s1 _ s2 _ s3) = list23 s1 ++ list23 s2 ++ list23 s3

eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 a b = list23 a == list23 b
    

-- ������ 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23  = undefined

-- isTerminal tr = True <=> ���� ���� ����� tr - ������ !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- ��������� ������� ����� � 2-3-������, 
--   ����� ����� - ����� ���� Node2 ��� Node3 � ��`��� �� (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - ��������� ������� - ���� 2-3-������ a 
--   : (a, Just (w, b)) - ��������� ������� ��� 2-3-������ a i b (w - �������� �������� � b)
--  insert v tr - ���� �������� v � ������� ������ tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - �������� �������� v � ������ tr � ����� - ����������� ����� 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tr - ���� �������� v � ������ tr � ������ - ������������� ����� 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined

---  ������� ������ 
bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB 
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)

---- 2-3-������
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )