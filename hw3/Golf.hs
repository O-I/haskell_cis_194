module Golf where

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n a = map fst [x | x <- zip a [1..length a], (snd x) `mod` n == 0]