import Data.List
import Data.List.Split

main = do
    input <- readFile "20.input"
    print $ day20a input
    print $ day20b input

day20a = fst . minimumBy f . zip [0..] . parse where
    f (n,[p0,v0,a0]) (m,[p1,v1,a1]) = compare (manhattan a0) (manhattan a1)
    manhattan = sum . map abs 

day20b = length . run 100 . parse

run 0 m = m
run n m = run (n-1) $ map tick $ filter noCollision m where
    noCollision [p,v,a] = length (elemIndices p (map head m)) == 1

tick [p,v,a] = [p',v',a] where
    v' = zipWith (+) v a
    p' = zipWith (+) p v'

parse = map parseLine . lines

parseLine = map parsePart . splitOn " "

parsePart :: String -> [Int]
parsePart = map read . take 3 . tail . splitOneOf "<>,"

