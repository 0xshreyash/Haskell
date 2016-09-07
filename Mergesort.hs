merge:: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
	      | x<= y = x:merge (xs) (y:ys)
              | otherwise = y:merge (x:xs) ys

mergePass:: Ord a => [[a]] -> [[a]] 
mergePass [] = []
mergePass [x] = [x]
mergePass (xs:ys:zss) = merge xs ys: mergePass zss

mergeRepeat:: Ord a => [[a]] -> [a]
mergeRepeat [] = []
mergeRepeat [x] = x
mergeRepeat x = mergeRepeat (mergePass x)

mergeSort:: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort xs = mergeRepeat [[x] | x <- xs]
