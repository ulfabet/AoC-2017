import Data.List
import Data.List.Split

main = do
	input <- readFile "9.input"
	-- input <- readFile "9b.example"
	print $ part2 input

part2 = map process . lines
process = sum . parse 0

parse i ('{':xs) = parse (i+1) xs
parse i ('}':xs) = parse (i-1) xs
parse i ('<':xs) = c:parse i ys where (c,ys) = garbage 0 xs
parse i (_:xs) = parse i xs
parse i [] = []

garbage c ('>':xs) = (c,xs)
garbage c ('!':_:xs) = garbage c xs
garbage c (_:xs) = garbage (c+1) xs
