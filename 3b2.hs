import Data.List

main = do
	print $ part2

part2 = take 9 spiral

spiral = generate (0,0) 0 0 0 0 

generate (x,y) d j s i = (x,y,s):generate (x',y') d' j' s' i' where
	(x',y') = move (x,y) d
	(d',j',i') = if j == 0 then (mod (d+1) 4, s, i-1) else (d, j-1, i)
	s' = if i == 0 then s+1 else s
	
move (x, y) 0 = (x+1, y)
move (x, y) 1 = (x, y+1)
move (x, y) 2 = (x-1, y)
move (x, y) 3 = (x, y-1)

