module SimpleFunctions where

filterFirst :: (a -> Bool) ->[a] -> [a]
filterFirst  _ [] = []
filterFirst  p (x:xs)  
    | p x         = x: filterFirst p xs
    | otherwise = xs



filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast  p (x:xs) = reverse (filterFirst p (reverse (x:xs)))

everyOther _ [] = []
everyOther 0 (x:[]) = x:[]
everyOther 1 (x:[]) = []
everyOther 0 (x:y:xs) = x: everyOther 0 xs
everyOther 1 (x:y:xs) = y: everyOther 1 xs	


split :: [a] -> ([a],[a])
split x = (everyOther 0 x, everyOther 1 x)
		

interleave :: ([a],[a]) -> [a]
interleave ([], []) = []
interleave ([], y) = y
interleave (x, []) = x
interleave ((x:xs), (y:ys)) = x:y:interleave(xs,ys)

merge :: (Ord a) => ([a],[a]) -> [a]

merge ((x), []) = x
merge ([], (y)) = y
--merge ([], []) = []
merge ( f@(x:xs), s@(y:ys) )
 | x < y = x:merge(xs, s)
 | otherwise = y:merge(f, ys)


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = x:[]
mergeSort x = 
 let (l, r) = split x
     left = mergeSort l
     right = mergeSort r
 in merge (left, right)
