main = do
	-- input <- readFile "1a.example"
	input <- readFile "1.input"
	print $ part1 input

part1 :: String -> [Int]
part1 = map process . lines 

process :: String -> Int
process (x:xs) = calc (last xs:x:xs)

calc :: String -> Int
calc (a:b:xs) = (if a == b then read [a] else 0) + calc (b:xs)
calc _ = 0
