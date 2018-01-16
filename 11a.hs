import Data.List
import Data.List.Split

main = do
	input <- readFile "11.input"
	-- input <- readFile "11a.example"
	print $ part1 input

part1 = map process . map (splitOn ",") . lines

process line = dist $ foldl (\acc dir -> move acc dir) (0,0) line

dist (x,0) = abs x
dist (0,y) = abs y
dist (x,y) | y > 0 = 1 + dist (x',y') where
	x' = x - (sign x)
	y' = y - if odd x then 1 else 0
dist (x,y) | y < 0 = 1 + dist (x',y') where
	x' = x - (sign x)
	y' = y + if odd x then 0 else 1

sign x | x == 0 = 0
       | x > 0 = 1
       | x < 0 = -1

move (x,y) "ne" = (x',y') where
	x' = x + 1
	y' = y + if odd x then 0 else 1
move (x,y) "nw" = (x',y') where
	x' = x - 1
	y' = y + if odd x then 0 else 1
move (x,y) "se" = (x',y') where
	x' = x + 1
	y' = y - if odd x then 1 else 0
move (x,y) "sw" = (x',y') where
	x' = x - 1
	y' = y - if odd x then 1 else 0

move (x,y) "n" = (x,y+1) where
move (x,y) "s" = (x,y-1) where

move (x,y) _ = (x,y)

