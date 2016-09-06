most_frequent :: [String] -> [(Char,Int)]
most_frequent [] = []
most_frequent (x:xs) = [ (y,c) | y<-['A'..'Z']++['a'..'z'], let c = (length.filter (==y))  x, c>0 ].most_frequent(xs)

