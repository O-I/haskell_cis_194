-- Exercise 5

-- Moves a stack of discs from first peg to second
-- where s = source, d = destination, u = using
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n s d u = hanoi (n-1) s u d ++ [(s, d)] ++ hanoi (n-1) u d s