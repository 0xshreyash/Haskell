import Data.List

type Colouring = [Colour]
data Colour = Blue | Green | Red
				deriving (Eq, Show)

graphColouring :: (Int, [(Int, Int)]) -> Colouring -> Bool
graphColouring (_, []) col = True
graphColouring (n, ((x, y):xs)) col = ((col !! (x-1)) /= (col !! (y-1))) && graphColouring (n, xs) col



--checkWellFormed ::  (Int, [(Int, Int)]) -> Bool
--checkWellFormed (_, []) = True
--checkWellFormed (n,((x, y):xs)) = (x>=0) && (y>=0) && (x <= n) && (y<= n) && checkWellFormed (n, xs)