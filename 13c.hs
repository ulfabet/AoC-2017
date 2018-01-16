import Data.List

main = do
	-- input <- readFile "13a.example"
	input <- readFile "13.input"
	print $ part2 input

part2 input = s where
	fw = map process $ lines input
	s = canPass fw 0

canPass fw t = if not . elem 0 $ map (getPos t) fw then t else canPass fw (t+1)

process :: String -> [Int] -- [depth, range]
process line = map read . words $ filter (/=':') line

getPos t (d:r:_) = p' where
	n = div (t+d) (r-1)
	m = mod (t+d) (r-1)
	p' = if even n then m else (r-1-m)
