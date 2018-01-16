import Data.List
import Data.List.Split
import Data.Char
import Data.Bits
import Text.Printf

main = do
	-- input <- readFile "10a.example"
	input <- readFile "10.input"
	print $ part2 input

hex :: Int -> String
hex i = printf "%02X" i

part2 input = concat $ map hex dense where
	list = (++ [17,31,73,47,23]) . filter (/=10) $ map ord input
	(sparse,_,_) = last . take 65 $ iterate (process list) ([0..255],0,0) where
	dense = map (foldr xor (0 :: Int)) $ divvy 16 16 sparse

process [] (buf,i,skip) = (buf,i,skip)
process (x:xs) (buf,i,skip) = process xs (buf',(mod (i+x+skip) 256),(skip+1)) where
	b = take 256 (drop i (cycle buf))
	e = cycle ((reverse (take x b)) ++ (drop x b))
	buf' = take 256 (drop (256 - (mod i 256)) e)

