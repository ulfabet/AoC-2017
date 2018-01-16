import Data.List

main = do
	input <- readFile "4.input"
	print $ process part1 input
	print $ process part2 input

process f = length . filter f . map words . lines
part1 xs = length xs == length (nub xs)
part2 = part1 . map sort 

