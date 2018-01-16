{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.List.Split (splitOn)

main = do
    -- input <- readFile "24.input"
    input <- readFile "24.example"
    let bridges = combinations 0 [] parts where
        parts = map parseLine $ lines input
        parseLine = map read . splitOn "/"
    -- print $ day24a bridges
    -- print $ day24b bridges
    mapM_ print bridges

day24a = maximum . map (sum . map sum)
day24b = maximum . map (\a -> (length a, sum $ map sum a))

combinations :: Int -> [[Int]] -> [[Int]] -> [[[Int]]]
combinations t xs ys | null $ filter (elem t) ys = [xs]
combinations t xs ys = concatMap f $ filter (elem t) ys where
    f part = combinations (other t part) (part:xs) (delete part ys)

-- combinations :: Int -> [[Int]] -> [[Int]] -> [[[Int]]]
-- combinations t xs ys = foldl' f [xs] (filter (elem t) ys) where
--     f z part = z ++ combinations (other t part) (part:xs) (delete part ys)

other side [a,b] | side == a = b
                 | otherwise = a
