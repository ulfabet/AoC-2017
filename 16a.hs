import Data.List
import Data.List.Split
import Data.Char

main = do
    input <- readFile "16.input"
    print $ day16a ['a'..'p'] input
    -- input <- readFile "16a.example"
    -- print $ day16a ['a'..'e'] input

day16a g = foldl' dance g . splitOn "," . head . lines

dance g ('s':xs) = take len . drop (len-n) $ cycle g where n = readInt xs; len = length g
dance g ('x':xs) = exchange g n m where [n,m] = map readInt $ splitOn "/" xs
dance g ('p':xs) = exchange g n m where
    [a,b] = map head $ splitOn "/" xs
    [n,m] = map (maybe 0 id) [elemIndex a g, elemIndex b g]
dance g move = g

exchange g n m = b where
    a = [ if i==n then g !! m else x | (x,i) <- zip g [0..] ]
    b = [ if i==m then g !! n else x | (x,i) <- zip a [0..] ]

readInt :: String -> Int
readInt = read . filter isNumber
