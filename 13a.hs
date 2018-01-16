import Data.List

main = do
	-- input <- readFile "13a.example"
	input <- readFile "13.input"
	print $ part1 input

part1 input = sum s where
	fw = map process $ lines input
	s = map severity fw

severity (d:r:p:[]) = if p' == 0 then d*r else 0 where
	p' = getPos d (d:r:p:[])

process :: String -> [Int] -- [depth, range, pos]
process line = (++ [0]) . map read . words $ filter (/=':') line

getPos t (d:r:p:[]) = p' where
	n = div t (r-1)
	m = mod t (r-1)
	p' = if even n then m else (r-1-m)
