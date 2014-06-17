module Golf where

-- Exercise 1  Hopscotch

skips :: [a] -> [[a]]
skips l = map (\n -> everyNth n l) [1..length l]

everyNth :: Int -> [a] -> [a]
everyNth n a
  | n <= 0    = []
  | null a    = []
  | otherwise = map fst [x | x <- zip a [1..length a], (snd x) `mod` n == 0]