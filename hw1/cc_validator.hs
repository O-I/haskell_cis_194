-- Exercise 1

-- Find the digits of a number
toDigits :: Integer -> [Integer]
toDigits cc
  | cc <= 0   = []
  | otherwise = toDigits (cc `div` 10) ++ [cc `mod` 10]

-- Find the digits of a number in reverse
toDigitsRev :: Integer -> [Integer]
toDigitsRev cc
  | cc <= 0   = []
  | otherwise = cc `mod` 10 : toDigitsRev (cc `div` 10)

-- Exercise 2

-- Double every second digit from the left
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft (x:y:xs) = x : 2 * y : doubleEveryOtherFromLeft xs
doubleEveryOtherFromLeft a = a

-- Double every second digit from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherFromLeft (reverse x))