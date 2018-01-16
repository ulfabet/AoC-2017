import Data.List
import Data.Char
import qualified Data.Traversable as T
import qualified Data.Map as Map

main = do
    -- input <- readFile "7a.example"
    input <- readFile "7.input"
    print $ day7a input
    -- T.mapM (putStrLn . show) $ day7a input

-- day7a input = maximum $ process m where
day7a input = sort $ balance m where
    m = foldl' parse Map.empty . map words . lines $ filter (/=',') input

process m = map (\k -> (depth m 0 k, k)) $ Map.keys m

-- balance m = map (\k -> (depth m 0 k, k)) $ Map.keys m
balance m = map (\k -> (weight m k, k)) $ Map.keys m

weight m k = if null children then w else w + (sum $ map (weight m) children) where
    (w,children) = Map.findWithDefault (0,[]) k m

depth m d k = if null children then d else maximum $ map (depth m (d+1)) children where
    (w,children) = Map.findWithDefault (0,[]) k m

parse m (n:w:"->":xs) = m' where
    m' = Map.insert n (readInt w, xs) m
parse m (n:w:[]) = m' where
    m' = Map.insert n (readInt w, []) m

readInt :: String -> Int
readInt = read . filter isNumber
