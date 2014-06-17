module Golf where

{- Exercise 1  Hopscotch

   The output of skips is a list of lists. The first list
   in the output should be the same as the input list. The
   second list in the output should contain every second
   element from the input list, and the nth list in the
   output should contain every nth element from the input list.

   I first implemented an everyNth function that takes an integer
   n and a list a and returns a list of every nth element of a.
   For n <= 0 or null a, an empty list is returned. Otherwise,
   I take the list and zip it with [1..length a] to act as a
   1-based pseudo index. I use a list comprehension to take only
   those ordered pairs where the index equals 0 mod n, and then
   map the first element of each pair to get the desired result.

   Finally, the function skips takes a list l and maps the
   lambda (everyNth n l) onto n in [1..length l].
-}

skips :: [a] -> [[a]]
skips l = map (\n -> everyNth n l) [1..length l]

everyNth :: Int -> [a] -> [a]
everyNth n a
  | n <= 0    = []
  | null a    = []
  | otherwise = map fst [x | x <- zip a [1..length a], (snd x) `mod` n == 0]