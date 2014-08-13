{-#LANGUAGE ParallelListComp #-} -- for fibs''

-- Exercise 1

fib :: Integer -> Integer
fib n
  | n == 0    = 0
  | n == 1    = 1
  | n  < 0    = -(fib (-n)) 
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

memo_fib :: Int -> Integer
memo_fib = (map fibo [0..] !!)
  where fibo 0 = 0
        fibo 1 = 1
        fibo n = memo_fib (n - 1) + memo_fib (n - 2)

fibs2 :: [Integer]
fibs2 = map memo_fib [0..]

-- Some alternatives found online:
-- (http://www.techrepublic.com/article/infinite-list-tricks-in-haskell/)

fibs' :: [Integer]
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

fibs'' :: [Integer]
fibs'' = 0 : 1 : [ a+b | a <- fibs'' | b <- tail fibs'' ]

fibs''' :: [Integer]
fibs''' = 0 : scanl (+) 1 fibs'''