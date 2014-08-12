-- Exercise 1

fib :: Integer -> Integer
fib n
  | n == 0    = 0
  | n == 1    = 1
  | n  < 0    = -(fib (-n)) 
  | otherwise = fib (n - 1) + fib (n - 2)