import Data.List

main = do
	-- input <- readFile "13a.example"
	input <- readFile "13.input"
	print $ part2 input

part2 input = s where
	fw = map process $ lines input
	s = length . takeWhile (==True) $ map (\t -> (or $ map (caught t) fw)) [0..]

severity t (d:r:p:[]) = if p' == 0 then d*r else 0 where
	p' = getPos (t+d) (d:r:p:[])

caught t (d:r:p:[]) = (p' == 0) where
	p' = getPos (t+d) (d:r:p:[])

process :: String -> [Int] -- [depth, range, pos]
process line = (++ [0]) . map read . words $ filter (/=':') line

update t (d:r:p:[]) = (d:r:p':[]) where
	n = div t (r-1)
	m = mod t (r-1)
	p' = if even n then m else (r-1-m)

getPos t (d:r:p:[]) = p' where
	n = div t (r-1)
	m = mod t (r-1)
	p' = if even n then m else (r-1-m)
