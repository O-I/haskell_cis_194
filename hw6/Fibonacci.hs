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

-- Exercise 3

data Stream a = Cons a (Stream a)
  deriving (Eq)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs