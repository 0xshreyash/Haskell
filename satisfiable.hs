satisfiable :: (Bool -> Bool -> Bool -> Bool) -> Bool
satisfiable f = or [ f p q r | p <- b, q <- b, r <- b]
                where b = [True, False]
