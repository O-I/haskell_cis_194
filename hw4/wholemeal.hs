-- Exercise 1

-- Original fun1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- Refactored fun1

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even