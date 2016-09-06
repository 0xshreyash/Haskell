checkWellFormed ::  (Int, [(Int, Int)]) -> Bool
checkWellFormed (_, []) = True
checkWellFormed (n,((x, y):xs)) = (x>=0) && (y>=0) && (x <= n) && (y<= n) && checkWellFormed (n, xs)