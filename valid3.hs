
valid3::(Bool -> Bool -> Bool) -> Bool 
valid3 f = and [f p q | p <- b, q <- b] 
            where b = [True, False] 
