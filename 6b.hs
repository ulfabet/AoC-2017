import Data.List

main = do
	-- input <- readFile "6a.example"
	input <- readFile "6.input"
	print $ part1 input

part1 xs = run (map read $ words xs) []

run x xs = if (length is) > 1 then is else run y ys where
	is = elemIndices x xs
	y = process x
	ys = y:xs

process xs = zipWith (+) zs (ror addMe (j+1)) where
	len = length xs
	ys = zip xs (reverse [0..len-1])
	(m,i) = maximum ys
	j = (len-1) - i
	zs = map (\(a,b) -> if b == i then 0 else a) ys
	addMe = map (\a -> (div m len) + (if a <= (mod m len) then 1 else 0)) [1..len]

rol xs n = take (length xs) (drop n (cycle xs))
ror xs n = rol xs ((length xs)-n)

