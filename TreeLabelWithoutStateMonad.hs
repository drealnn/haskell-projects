module TreeLabelWithoutStateMonad where

import Store

-- label element

labelValue :: Ord a => a -> (Store a Int) -> (Int, Store a Int)
labelValue val ls
 | lookupStore val ls == Nothing = ((createNewLabel ls), (insertStore val (createNewLabel ls) ls))
 | otherwise = (((\(Just x) -> x) (lookupStore val ls)),(ls))


-- where
 -- let newVal1 = lookupStore val ls in
  -- if (newVal1 == Nothing) then do 
   --  newVal = (createNewLabel ls)			
    -- newStore = insertStore val newVal1 ls
   --else 
    -- newVal = (\(Just x) -> x) newVal1
     --newStore = ls
		
		
		  -- replace this stub implementation by a proper implementation of labelValue

-- label tree

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Eq)

labelTree :: Ord a => Tree a -> (Store a Int) -> (Tree Int, Store a Int)
labelTree Nil ls = (Nil, ls)
  
labelTree (Node val left right) ls =
  (Node labeledValue labeledLeft labeledRight, ls''')
    where (labeledValue, ls')   = labelValue val   ls
          (labeledLeft,  ls'')  = labelTree  left  ls'
          (labeledRight, ls''') = labelTree  right ls''

getLabeledTree :: Ord a => Tree a -> Tree Int
getLabeledTree tree = fst $ labelTree tree emptyStore
