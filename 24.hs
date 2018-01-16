import Data.List
import Data.List.Split

main = do
    input <- readFile "24.example"
    -- input <- readFile "24.input"
    -- print $ day24a input
    -- mapM_ print $ day24a input
    --
    -- day24b
    print $ maximum . map (sum . map sum) $ day24a input
    --
    -- day24b
    print . last . sort . map (\a -> (length a, sum $ map sum a)) $ day24a input

day24a input = combinations 0 [] ports where
    ports = map parseLine $ lines input
    parseLine = map readInt . splitOn "/"

readInt :: String -> Int
readInt = read

combinations t xs [] = [xs]
combinations t xs ys = zs' where
    zs = filter (elem t) ys
    zs' = if length zs == 0 then [xs] else concat $ map f zs
    f port = combinations (other t port) (port:xs) (delete port ys)

other side [a,b] | side == a = b
                 | otherwise = a
