-- Based on Haskell - the craft of functional programming
-- section 16.7 Search tree
-- This implements a binary search tree with a minimal API
--
-- Check out http://algs4.cs.princeton.edu/32bst/ for an
-- implementation in Java with a much more extensive API

module SearchTree
 (Tree(..),   -- Nil | Node
  nil,        -- Tree a
  isNil,      -- Tree a -> Bool
  isNode,     -- Tree a -> Bool
  leftSub,    -- Tree a -> Tree a
  rightSub,   -- Tree a -> Tree a
  treeVal,    -- Tree a -> a
  insTree,    -- Ord a => a -> Tree a -> Tree a
  delete,     -- Ord a => a -> Tree a -> Tree a
  minTree,    -- Ord a => Tree a -> Maybe a
  successor,  -- Ord a => a -> Tree a -> Maybe a
  closest,    -- Int -> Tree Int -> Int
 ) where 

import Data.Maybe

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq) -- not deriving show because I wrote my own show function
           
nil :: Tree a
nil = Nil

insTree :: Ord a => a -> Tree a -> Tree a

insTree val Nil = (Node val Nil Nil)
insTree val (Node v t1 t2)
  | v == val  = Node v t1 t2
  | (val > v) = Node v t1 (insTree val t2)
  | (val < v) = Node v (insTree val t1) t2

delete :: Ord a => a -> Tree a -> Tree a

delete val Nil = Nil
delete val (Node v t1 t2)
  | (val > v) = Node v t1 (delete val t2)
  | (val < v) = Node v (delete val t1) t2
  | isNil t2  = t1
  | isNil t1  = t2
  | otherwise = join t1 t2

minTree :: Ord a => Tree a -> Maybe a
minTree Nil   = Nothing
minTree (Node v t1 _)
  | isNil t1  = Just v
  | otherwise = minTree t1

join :: (Ord a) => Tree a -> Tree a -> Tree a
join t1 t2 =
  Node mini t1 newt
    where
      (Just mini) = minTree t2
      newt        = delete mini t2

isNil :: Tree a -> Bool
isNil Nil = True
isNil _   = False

isNode :: Tree a -> Bool
isNode Nil          = False
isNode (Node _ _ _) = True

leftSub :: Tree a -> Tree a
leftSub Nil           = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub :: Tree a -> Tree a
rightSub Nil           = error "rightSub"
rightSub (Node _ _ t2) = t2

treeVal :: Tree a -> a
treeVal Nil          = error "treeVal"
treeVal (Node v _ _) = v

-- add your solutions here



myMin :: (Ord a) => Maybe a -> Maybe a -> Maybe a
myMin x Nothing = x
myMin Nothing y = y
myMin x y = if x <= y then x else y

myMax :: (Ord a) => Maybe a -> Maybe a -> Maybe a
myMax x Nothing = x
myMax Nothing y = y
myMax x y = if x >= y then x else y

maxTree :: Ord a => Tree a -> Maybe a
maxTree Nil   = Nothing
maxTree (Node v _ t1)
  | isNil t1  = Just v
  | otherwise = maxTree t1

successor :: Ord a => a -> Tree a -> Maybe a
--successor _ Nil = Nothing
-- get min of root value and left branch if root is greater than the node
successor n (Node v left right) 
 | v < n && ((isNil right) ) = Nothing
 | v < n = successor n right -- recur until we find a node greater than n
 | v > n && ((isNil left) )  = Just v
 | v > n  = myMin (Just v) (successor n left)
 | v == n = minTree right

predecessor :: Ord a => a -> Tree a -> Maybe a
predecessor n (Node v left right) 
 | v > n && ((isNil left) ) = Nothing
 | v > n = predecessor n left -- recur until we find a node less than n
 | v < n && ((isNil right) )  = Just v
 | v < n  = myMax (Just v) (predecessor n right)
 | v == n =  Just v --maxTree left

closest :: Int -> Tree Int -> Int
closest n (Node v left right) = 
 let 
	succ = (successor n (Node v left right)) 
	pred = (predecessor n (Node v left right))
	succ' = if (succ == Nothing) then 0 else (\(Just x) -> x) succ
	pred' = if (pred == Nothing) then 0 else (\(Just x) -> x) pred
	sdiff = abs (succ' - n)
	pdiff = abs (pred' - n)
 in 
	if  sdiff < pdiff then succ' else pred'
 
-- code to display trees

pairEntriesWithDepth :: Tree a -> Int -> [(Maybe a, Int)]

pairEntriesWithDepth Nil depth                 = [(Nothing, depth)]
pairEntriesWithDepth (Node x left right) depth =
  (Just x,depth):(pairEntriesWithDepth left (depth + 1) ++ pairEntriesWithDepth right (depth + 1))

instance (Show a) => Show (Tree a) where
  show tree = init $ unlines [replicate d '.' ++ (show' n) | (n,d) <- pairEntriesWithDepth tree 0]
    where
      show' Nothing   = "nil"
      show' (Just x)  = show x
