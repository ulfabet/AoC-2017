--import Data.List
import qualified Data.Map as Map

main = do
	input <- readFile "5.input"
	-- input <- readFile "5a.example"
	print $ part1 input
	-- print $ process part2 input

-- process f = length . filter f . map words . lines
-- part1 xs = length xs == length (nub xs)
-- part2 = part1 . map sort 

-- part1 xs = length xs - sum as where
--
-- part1 xs = length as - sum as where
-- 	as = map read . lines $ xs

-- count :: Eq a => a -> [a] -> Int
-- count x = length . filter (==x)

-- process (i:is) xs = if i < 0 || i > (length xs)-1 then length (i:is)-1 else process (i':i:is) xs where
-- 	i' = i + (xs !! i) + count i is

-- part1 xs = process (0:[]) ys where
-- 	ys = map read . lines $ xs
-- part1 = process (0:[]) . map read . lines

-- part1 xs = process m 1 (length ys) 0 where
-- 	ys = map (+0) . map read . lines $ xs
-- 	m = Map.fromList $ zip [1..] ys


part1 xs = flip (-) 1 . length . takeWhile (/=Nothing) $ iterate f $ Just (m, 1) where
	ys = map (+0) . map read . lines $ xs
	m = Map.fromList $ zip [1..] ys
	f = process 

process Nothing = Nothing
process (Just (m, i)) = val where
	o = (m Map.! i)
	i' = i + o
	d = if o > 2 then (-1) else 1
	-- d = 1
	m' = Map.insert i (o+d) m
	val = if i < 1 || i > Map.size m then Nothing else Just (m', i')



-- process _ i j c
-- 	| i < 1 = c
-- 	| i > j = c
-- process m i j c = process m' i' j c' where
-- 	val = (m Map.! i)
-- 	i' = i + val
-- 	c' = c + 1
-- 	d = if val > 2 then (-1) else 1
-- 	m' = Map.insert i (val + d) m


-- process m i j c = if i < 1 || i > j then c else process m' i' j c' where
-- 	val = (m Map.! i)
-- 	i' = i + val
-- 	c' = c + 1
-- 	d = if val > 2 then (-1) else 1
-- 	m' = Map.insert i (val + d) m

-- process i c xs = if i < 0 || i > (length xs)-1 then c else process i' c' xs' where
-- 	i' = i + (xs !! i)
-- 	c' = c + 1
-- 	(as, b:bs) = splitAt i xs
-- 	xs' = as ++ b+(if b >= 3 then (-1) else 1):bs
