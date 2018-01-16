import Data.List
import qualified Data.Map as Map

main = do
	-- input <- readFile "12a.example"
	input <- readFile "12.input"
	print $ part2 input

part2 input = length $ groups m (Map.keys m) where
	m = foldl' process Map.empty (lines input)

process m line = Map.insert x xs m where
	(x:"<->":xs) = words $ filter (/=',') line

build m s x = if elem x s
		then s
		else foldl' (build m) (x:s) (maybe [] id (Map.lookup x m))

groups m [] = []
groups m (x:xs) = s:groups m ys where
	s = build m [] x
	ys = xs \\ s
