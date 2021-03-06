module Golf where

import Data.List

{- Exercise 1  Hopscotch

   The output of skips is a list of lists. The first list
   in the output should be the same as the input list. The
   second list in the output should contain every second
   element from the input list, and the nth list in the
   output should contain every nth element from the input list.

   First, I implemented an everyNth function that takes an integer
   n and a list a and returns a list of every nth element of a.
   For null a, an empty list is returned. For n <= 0, an invalid
   size error is raised. Otherwise, I take the list and zip it with
   [1..length a] to act as a 1-based pseudo index. I use a list
   comprehension to take only those ordered pairs where the index
   equals 0 mod n, and take only the first element of each pair to
   get the desired result.

   Finally, the function skips takes a list l and maps the
   lambda (everyNth n l) onto n in [1..length l].
-}

skips :: [a] -> [[a]]
skips l = map (\n -> everyNth n l) [1..length l]

everyNth :: Int -> [a] -> [a]
everyNth n a
  | n <= 0    = error "Size must be an integer greater than zero"
  | null a    = []
  | otherwise = [ x | (x,y) <- zip a [1..length a], y `mod` n == 0 ]

{- Exercise 2  Local maxima

   A local maximum of a list is an element of the list which is
   strictly greater than both the elements immediately before and
   after it. For example, in the list [2,3,4,1,5], the only local
   maximum is 4, since it is greater than the elements immediately
   before and after it (3 and 1). 5 is not a local maximum since
   there is no element that comes after it.

   First, I implemented an eachCons function modeled after Ruby's
   Enumerable#each_cons. Given a positive integer n and a list l,
   eachCons n l returns a list of lists that are consecutive n
   elements of l. For null l, an empty list is returned and an
   error is raised if n <= 0.

   Finally, localMaxima takes an integer list l and returns a list
   of all local maxima (if there are any) using a list comprehension
   ranging over all triples of eachCons 3 l such that the second
   element is greater than the first and the third.
-}

localMaxima :: [Int] -> [Int]
localMaxima l = [ y | [x,y,z] <- eachCons 3 l, y > x, y > z ]

eachCons :: Int -> [a] -> [[a]]
eachCons n a
  | n <= 0    = error "Size must be an integer greater than zero"
  | null a    = []
  | otherwise = map (\x -> take n $ drop x a) [0..length a - n]

{- Exercise 3  Histogram

   Write a function which takes as input a list of Integers between
   0 and 9 (inclusive), and outputs a vertical histogram showing how
   many of each number were in the input list. You may assume that the
   input list does not contain any numbers less than zero or greater
   than 9 (that is, it does not matter what your function does if the
   input does contain such numbers).
-}

histogram :: [Integer] -> String
histogram l = intercalate "\n" (squeeze $ rotNeg90 $ pad $ asterisk $ freq l) ++ "\n"

squeeze :: [String] -> [String]
squeeze l = dropWhile (== replicate 10 ' ') l

rotNeg90 :: [String] -> [String]
rotNeg90 l = reverse $ transpose l

pad :: [String] -> [String]
pad l = map (\s -> s ++ replicate (10 - length s) ' ') l

asterisk :: [Integer] -> [String]
asterisk l = map (\(m,n) -> (show m) ++ "=" ++ replicate (fromIntegral n) '*') (zip [0..9] l)

freq :: [Integer] -> [Integer]
freq l = map (\n -> count n l) [0..9]

count :: Integer -> [Integer] -> Integer
count _ [] = 0
count n (x:xs)
  | n == x    = 1 + count n xs
  | otherwise = count n xs