-- Exercise 5

-- Moves a stack of discs from first peg to second
-- where s = source, d = destination, u = using
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n s d u
  | n <= 0 = []
  | n >  0 = hanoi (n-1) s u d ++ [(s, d)] ++ hanoi (n-1) u d s

-- Exercise 6

-- Four-peg Towers of Hanoi (not optimal)
-- Moves a stack of discs from first peg to last
-- where s = source, x = using1, y = using2, d = destination
hanoi4Peg :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4Peg n s x y d
  | n <= 0 = []
  | n == 1 = [(s, d)]
  | n >  1 = hanoi4Peg (n-2) s y d x ++ [(s, y), (s, d), (y, d)] ++ hanoi4Peg (n-2) x s y d

-- Reve's problem solved optimally using the Frame-Stewart algorithm[1]
reve :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
reve n s x y d
  | n <= 0 = []
  | n == 1 = [(s, d)]
  | n >  1 = reve (n - k) s y d x ++ hanoi k s d y ++ reve (n - k) x s y d
  where k = floor (sqrt (2.0 * fromIntegral n) + 0.5)

{- [1] References

  The following link (http://everything2.com/title/Reve%2527s+puzzle) has a very approachable breakdown of solving Reve's puzzle using the Frame-Stewart algorithm. However, unless I am misunderstanding, the assertion of k = n/2 as optimal is incorrect.

  This paper by Michael Rand (https://www2.bc.edu/~grigsbyj/Rand_Final.pdf), which is more rigorous but harder to follow, makes it clear that the optimal k for n discs in the 4-peg problem is floor(sqrt(2*n) + 1/2).

  Further reading:

  http://demonstrations.wolfram.com/TheRevesPuzzle/
  http://mathworld.wolfram.com/TowerofHanoi.html
  http://oeis.org/A007664

-}