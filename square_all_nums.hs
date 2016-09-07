square::Integer -> Integer
square n = n*n

square_all_nums::[Integer] -> [Integer]
square_all_nums (xs) = map square (xs)
