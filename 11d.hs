import Data.List
import Data.List.Split

main = do
	input <- readFile "11.input"
	-- input <- readFile "11a.example"
	print $ part1 input

part1 = map process . map (splitOn ",") . lines

process = head . map dist . foldl' (\(a:acc) dir -> (move a dir):a:acc) [(0,0,0)]

move (a,b,c) "ne" = (a+1,b,c)
move (a,b,c) "sw" = (a-1,b,c)
move (a,b,c) "nw" = (a,b+1,c)
move (a,b,c) "se" = (a,b-1,c)
move (a,b,c) "n" = (a,b,c+1)
move (a,b,c) "s" = (a,b,c-1)

dist (a,b,c) = abs a' + abs b' + abs c' where
	(a',b',c') = conv (a,b,c)

conv (a,0,0) = (a,0,0)
conv (0,b,0) = (0,b,0)
conv (0,0,c) = (0,0,c)
conv (a,b,c)
	| sign a == sign b = conv (a-sign a, b-sign b, c+sign a)
	| sign a == -sign c = conv (a-sign a, b+sign c, c-sign c)
	| sign b == -sign c = conv (a+sign c, b-sign b, c-sign c)
conv (a,b,c) = (a,b,c)

sign x	| x == 0 = 0
	| x > 0 = 1
	| x < 0 = -1

