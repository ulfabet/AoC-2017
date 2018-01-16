import Data.List
import Data.List.Split

main = do
	-- input <- readFile "11.input"
	input <- readFile "11a.example"
	print $ part1 input

part1 = map process . map (splitOn ",") . lines

process = head . foldl (\(a:acc) dir -> (move a dir):a:acc) [(0,0,0)]

dist2 (x,y) = abs x + max 0 ((abs y)-d) where
	d = div ((abs x)+1) 2

move (d,x,y) "ne" = (d',x',y') where
	x' = x + 1
	y' = y + if odd x then 0 else 1
	d' = dist2 (x',y')
move (d,x,y) "nw" = (d',x',y') where
	x' = x - 1
	y' = y + if odd x then 0 else 1
	d' = dist2 (x',y')
move (d,x,y) "se" = (d',x',y') where
	x' = x + 1
	y' = y - if odd x then 1 else 0
	d' = dist2 (x',y')
move (d,x,y) "sw" = (d',x',y') where
	x' = x - 1
	y' = y - if odd x then 1 else 0
	d' = dist2 (x',y')
move (d,x,y) "n" = (dist2 (x,y+1),x,y+1) where
move (d,x,y) "s" = (dist2 (x,y-1),x,y-1) where
move (d,x,y) _ = (d,x,y)

