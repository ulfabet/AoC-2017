main = do
	print $ part1

part1 = a + abs (b - a) where
	x = 312051 
	(n, s) = nextOddSquare x 1
	a = div n 2
	b = mod (s - x) (n-1)

nextOddSquare x n = if x < n^2 then (n, n^2) else nextOddSquare x (n+2)

