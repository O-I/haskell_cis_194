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

-- Exercise 3

-- Calculate the sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Exercise 4

-- Validate credit card number
validate :: Integer -> Bool
validate cc = luhnCheck cc && validLength cc

validLength :: Integer -> Bool
validLength x = (length (toDigits x)) `elem` [13..19]

luhnSum :: Integer -> Integer
luhnSum cc = sumDigits (doubleEveryOtherFromLeft (toDigitsRev cc))

luhnCheck :: Integer -> Bool
luhnCheck cc =  (luhnSum cc) `mod` 10 == 0