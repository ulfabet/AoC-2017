main = do
	print $ part2

part2 = take 85 (calc 1 [1])

calc pos xs = (val:calc (pos+1) (xs ++ [val])) where
	val = getValue2 pos xs

getValue2 1 _ = 1
getValue2 pos xs = val where
	(x, y) = getXY pos
	val = sum [ xs !! (getPos a b) | a <- [x-1,x,x+1], b <- [y-1,y,y+1], pos > getPos a b]


nextOddSquare x n = if x < n^2 then (n, n^2) else nextOddSquare x (n+2)

getValue 0 0 = 1
getValue x y = sum [ getValue a b | a <- [x-1, x, x+1], b <- [y-1, y, y+1], getPos x y > getPos a b ]

getXY 1 = (0,0)
getXY pos = (x, y) where
 	(n, s) = nextOddSquare (pos-1) 1
	m = div n 2

	k1 = n*n - 1*(n-1) 
	k2 = n*n - 2*(n-1) 
	k3 = n*n - 3*(n-1) 
	k4 = n*n - 4*(n-1) 

	a = s - div n 2
	b = a - 2 * (n-1)
	c = a - 3 * (n-1)
	d = a - 1 * (n-1)

	(x1, y1) = if pos >= k1 then (pos-a, -m) else (b-pos, m)
	(x2, y2) = if pos < k3 then (m, pos-c) else (-m, d-pos)

	(x, y) = if pos >= k1 || pos <= k2 && pos >= k3 then (x1, y1) else (x2, y2)

getPos 0 0 = 1
getPos x y = p where 
	m = maximum (map abs [x, y])
	n = 1 + 2 * m
	c = 4*(n-1)
	d = (n*n - c) + (div n 2)
	e = (n-1)
	ky = if y == m then -1 else 1
	kx = if x == m then -1 else 1
	p = if abs(y) == m
		then d + (2+ky)*e + ky*x
		else d + (1+kx)*e - kx*y

