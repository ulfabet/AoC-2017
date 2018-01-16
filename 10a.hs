import Data.List
import Data.List.Split

main = do
	input <- readFile "10.input"
	-- input <- readFile "10a.example"
	print $ part1 input

part1 = process [] [0..255] 0 0 . map (+0) . map read . splitOn ","

process acc buf i skip [] = product (take 2 buf)
process acc buf i skip (x:xs) = process (buf:acc) buf' (i+x+skip) (skip+1) xs where
	b = take 256 (drop i (cycle buf))
	c = reverse (take x b)
	d = drop x b
	buf' = take 256 (drop (256 - (mod i 256)) (cycle (c ++ d)))
