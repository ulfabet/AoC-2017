import Data.List

main = do
	-- input <- readFile "6a.example"
	input <- readFile "6.input"
	print $ part1 input

-- part1 = take 6 . iterate process . map ((+0) . read) . words
part1 = run [] . map ((+0) . read) . words

run s banks = if elem banks s then length s else run (banks:s) (process banks)

process xs = zs where
	len = length xs
	(m,i) = maximum . zip xs $ reverse [1..len]
	j = len-i
	zs = map f (zip xs [0..]) where
		z = mod m len
		f (x,n) = (if n == j then 0 else x) + (div m len) +
			 if elem n (map (\a -> mod (j+a) len) [1..z]) then 1 else 0
