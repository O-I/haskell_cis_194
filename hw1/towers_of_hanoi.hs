-- Exercise 5

-- Moves a stack of discs from first peg to second
-- where s = source, d = destination, u = using
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n s d u
  | n <= 0 = []
  | n >  0 = hanoi (n-1) s u d ++ [(s, d)] ++ hanoi (n-1) u d s

-- Four-peg Towers of Hanoi (not optimal)
-- Moves a stack of discs from first peg to last
-- where s = source, x = using1, y = using2, d = destination
hanoi4Peg :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4Peg n s x y d
  | n <= 0 = []
  | n == 1 = [(s, d)]
  | n >  1 = hanoi4Peg (n-2) s y d x ++ [(s, y), (s, d), (y, d)] ++ hanoi4Peg (n-2) x s y d