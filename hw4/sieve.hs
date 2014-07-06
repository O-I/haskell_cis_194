-- Exercise 4  Finding primes

-- Given an integer n, generate all the odd primes
-- up to 2n + 2 using function composition

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x + 1 | x <- [1..n], not (x `elem` sieve n)]

sieve :: Integer -> [Integer]
sieve n = [i + j + 2*i*j | i <- [1..n], j <- [1..n], i + j + 2*i*j <= n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]