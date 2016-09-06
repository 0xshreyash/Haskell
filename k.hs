

 k :: Integer -> Integer -> Integer
 k n m                                         -- find K^n_m (n over m)
     = numerator `div` denominator
        where
        numerator   = product [n, n-1..(n-m+1)]
        denominator = product [1..m]


