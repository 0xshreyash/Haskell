factors :: Integer -> [Integer]
factors n = [x | x <- [2..n], x*x <= n, n `mod` x == 0]

prime :: Integer -> Bool 
prime n = length (factors n) == 0
