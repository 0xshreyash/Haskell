valid :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid f = and [ f p q r | p <- b, q <- b, r <- b]
            where b = [True, False]
