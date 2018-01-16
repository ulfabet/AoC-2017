import Data.List
import Data.List.Split
import Data.Char
import Data.Bits
import Text.Printf
import Numeric

-- group = [(x1,y1),...]
-- walkGroup :: group ... -> group'
-- findGroups :: ... -> [group1,group2,...]
-- hasVisited :: ... -> Bool
-- neighbours :: (x,y) ... -> group
-- valid :: (x,y) -> Bool

walkGroup m (x,y) g = if valid (x,y) then foldr (walkGroup m) ((x,y):g) h else g where
    h = filter valid $ neighbours (x,y)
    valid (x,y) = x >= 0 && y >= 0 && x <= 127 && y <= 127 && ((m !! y) !! x) == 1 && not (visited (x,y))
    neighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)] 
    visited (x,y) = elem (x,y) g

main = do
    -- input = "jxqlasbh" -- real
    -- let input = "flqrgnkx" -- example
    -- mapM_ putStrLn $ getGrid input
    ----
    input <- readFile "14b.grid"
    -- input <- readFile "14b.grid.example"
    let grid = lines input
    print $ part2 grid
    -- mapM_ (putStrLn . show) $ corner grid
    -- mapM_ (putStrLn . show) $ corner2 grid

-- part2 grid = walkGroup m (0,0) [] where
part2 grid = length $ findGroups m (0,0) [] where
    m = map expandRow grid

corner grid = take 8 $ map (take 8) m where
    m = map expandRow grid

corner2 grid = take 8 $ map (take 2) grid

findGroups m (x,128) gs = gs
findGroups m (x,y) gs = findGroups m (x',y') gs' where
    -- gs = filter (==[]) [ walkGroup m (x,y) [] | x <- [0..127], y <- [0..127] ]
    g = if not (inGroups (x,y)) then walkGroup m (x,y) [] else []
    gs' = if null g then gs else g:gs
    inGroups (x,y) = or $ map (elem (x,y)) gs
    (x',y') = (mod (x+1) 128, if x' == 0 then y+1 else y)

expandRow :: String -> [Int]
expandRow (x:xs) = a:b:c:d:expandRow xs where
    y = digitToInt x
    a = if y .&. 8 == 8 then 1 else 0
    b = if y .&. 4 == 4 then 1 else 0
    c = if y .&. 2 == 2 then 1 else 0
    d = if y .&. 1 == 1 then 1 else 0
expandRow _ = []

--------

part1 input = sum $ map countBits grid where
    rows = map (\a -> input ++ "-" ++ (show a)) [0..127]
    grid = map knotHash rows

countBits = sum . map (popCount . digitToInt)

-- getGrid input = grid where
--     rows = map (\a -> input ++ "-" ++ (show a)) [0..127]
--     grid = map knotHash rows

knotHash input = concat $ map hex dense where
    list = (++ [17,31,73,47,23]) . filter (/=10) $ map ord input
    (sparse,_,_) = last . take 65 $ iterate (process list) ([0..255],0,0) where
    dense = map (foldr xor (0 :: Int)) $ divvy 16 16 sparse

hex :: Int -> String
hex i = printf "%02X" i

process [] (buf,i,skip) = (buf,i,skip)
process (x:xs) (buf,i,skip) = process xs (buf',(mod (i+x+skip) 256),(skip+1)) where
    b = take 256 (drop i (cycle buf))
    e = cycle ((reverse (take x b)) ++ (drop x b))
    buf' = take 256 (drop (256 - (mod i 256)) e)

