main = do
	input <- readFile "1.input"
	print . map part1 . lines $ input
	print . map part2 . lines $ input

part1 xs = calc xs (tail (xs ++ xs))

part2 xs = calc xs (drop n (xs ++ xs)) where
	n = div (length xs) 2

calc (x:xs) (y:ys) = (if x == y then read[x] else 0) + calc xs ys
calc _ _ = 0

