-- Exercise 5

-- Moves a stack of discs from first peg to second
-- where s = source, d = destination, u = using
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n s d u
  | n <= 0 = []
  | n >  0 = hanoi (n-1) s u d ++ [(s, d)] ++ hanoi (n-1) u d s