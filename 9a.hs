import Data.List
import Data.List.Split

main = do
	input <- readFile "9.input"
	-- input <- readFile "9a.example"
	print $ part1 input

part1 = map process . lines

process = sum . parse 0 . parseBang

parseBang ('!':x:xs) = '?':'?':parseBang xs
parseBang (x:xs) = x:parseBang xs
parseBang [] = []

parse i (x:xs)
	| x == '{' = parse (i+1) xs
	| x == '}' = i:parse (i-1) xs
	| x == '<' = parse i $ parseG xs
	| x == ',' = parse i xs
	| otherwise = parse i xs
parse i [] = []

parseG (x:xs)
	| x == '>' = xs
	| otherwise = parseG xs

