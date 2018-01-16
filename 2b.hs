main = do
	input <- readFile "2.input"
	print $ part2 input

part2 = sum . map process . lines

process xs = div a b where
	ys = map read . words $ xs
	[(a,b)] = [(a,b) | a <- ys, b <- ys, div a b > 1, mod a b == 0]
