main = do
	input <- readFile "2.input"
	print $ part1 input

part1 = sum . map process . lines

process xs = maximum ys - minimum ys where
	ys = map read . words $ xs
