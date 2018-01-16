import Data.List

main = do
	input <- readFile "5.input"
	print $ part2 input

part2 = process 0 0 . map (+0) . map read . lines

process i c xs = if i < 0 || i > (length xs)-1 then c else process i' c' xs' where
	i' = i + (xs !! i)
	c' = c + 1
	(as, b:bs) = splitAt i xs
	xs' = as ++ b+(if b >= 3 then (-1) else 1):bs
