import Data.List
import Data.List.Split

main = do
	input <- readFile "9.input"
	-- input <- readFile "9b.example"
	print $ part1 input

part1 = map process . lines

process = sum . parse 0

parse i (x:xs)
	| x == '{' = parse (i+1) xs
	| x == '}' = parse (i-1) xs
	| x == '<' = let (c,ys) = parseG 0 xs in c:parse i ys
	| x == ',' = parse i xs
	| otherwise = parse i xs
parse i [] = []

parseG c (x:xs)
	| x == '>' = (c,xs)
	| x == '!' = parseG c (tail xs)
	| otherwise = parseG (c+1) xs

