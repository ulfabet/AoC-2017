import Data.List
-- import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map

main = do
	-- input <- readFile "12a.example"
	input <- readFile "12.input"
	print $ part1 input

part1 xs = Set.size $ build s m "0" where
	s = Set.empty
	m = foldl' process Map.empty (lines xs)

process m line = m' where
	xs = words (filter (/=',') line)
	m' = Map.insert (head xs) (drop 2 xs) m

build s m x = s' where
	s' = if Set.member x s
		then s
		else foldr (\a z -> build z m a) (Set.insert x s) (maybe [] id (Map.lookup x m))

