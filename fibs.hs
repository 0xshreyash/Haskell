fibs::Integer -> Integer
fibs 0 = 1
fibs 1 = 1
fibs n = (fibs (n -1) + fibs (n - 2))

fib_series::Integer -> [Integer]
fib_series 0 = [1]
fib_series n = (fibs n):fib_series (n-1) 

