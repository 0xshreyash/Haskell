-- Join two lists together
merge:: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	      | x<= y = x:merge (xs) (y:ys)
              | otherwise = y:merge (x:xs) ys
-- join a pairs of lists togehter
mergePass:: Ord a => [[a]] -> [[a]] 
mergePass [] = []
mergePass [x] = [x]
mergePass (xs:ys:zss) = merge xs ys: mergePass zss
-- Keep on joining lists till we have only one list left
mergeRepeat:: Ord a => [[a]] -> [a]
mergeRepeat [] = []
mergeRepeat [x] = x
mergeRepeat x = mergeRepeat (mergePass x)
-- Creating a list of singletons to run mergeRepeat on
mergeSort:: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort xs = mergeRepeat [[x] | x <- xs]
