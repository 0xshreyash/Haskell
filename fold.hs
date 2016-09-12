import Prelude hiding (foldr, foldl)
foldr f z [] = z
fold f z (x:xs) = f x (foldr f z xs)

foldl f z [] = z
foldl f z (x:xs) = let z' = z `f` x in foldl f z' xs
