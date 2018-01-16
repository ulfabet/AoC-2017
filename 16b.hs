import Data.List
import Data.List.Split
import Data.Char

main = do
    input <- readFile "16.input"
    print $ day16b ['a'..'p'] input

day16b g input = take 41 $ iterate f g
    where f g = foldl' dance g . splitOn "," . head $ lines input

move ('s':xs) = f where
    f g = take len . drop (len-n) $ cycle g
    n = readInt xs
    len = 16

move ('x':xs) = f where
    f g = exchange g n m
    [n,m] = map readInt $ splitOn "/" xs

move ('p':xs) = f where
    [a,b] = map head $ splitOn "/" xs
    f g = exchange g n m where
        [n,m] = map (maybe 0 id) [elemIndex a g, elemIndex b g]

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
