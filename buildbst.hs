data Bintree a = Void | Node a (Bintree a) (Bintree a)
					deriving (Eq, Show)

buildbst :: [a] -> Bintree a
buildbst [] = Void
buildbst (x:xs) = if (Node )