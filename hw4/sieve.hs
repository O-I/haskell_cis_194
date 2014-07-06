-- Exercise 4  Finding primes

-- Given an integer n, generate all the odd primes
-- up to 2n + 2 using function composition

import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x + 1 | x <- [1..n], not (x `elem` sieve n)]

sieve :: Integer -> [Integer]
sieve n = [i + j + 2*i*j | i <- [1..n], j <- [1..n], i + j + 2*i*j <= n]

sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map (\x -> 2*x + 1) $ [1..n] \\ sieve' n

sieve' :: Integer -> [Integer]
sieve' n = filter (<= n) $ map (\(i,j) -> i + j + 2*i*j) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]