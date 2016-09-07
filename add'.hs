
add :: [Int] -> Int 
add [] = 0
add (x:xs) =  x + add xs 