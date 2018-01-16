import Data.List
import Data.Char
import qualified Data.Traversable as T
import qualified Data.Map as Map

-- [(name, weigth, [children])]
-- name -> (weight, children)
-- weight name = ...
-- 
-- getWeight :: name -> weight
-- getTotalWeight :: name -> weight
-- getChildren :: name -> children
--
-- Algorithm:
-- 1 compare total weight of children
-- 2 select child with different weight
-- 3 compare total weight of children 
-- 4 if no children or all equal then solution is current name else goto 2

main = do
    -- input <- readFile "7a.example"
    input <- readFile "7.input"
    print $ day7b input
    -- T.mapM (putStrLn . show) $ day7b input

day7b input = result where
    m = foldl' parse Map.empty . map words . lines $ filter (/=',') input
    root = snd . maximum . map (\k -> (depth m 0 k, k)) $ Map.keys m
    result = run m root 0
    
run m k d = result where
    w = weight m k
    cs = children m k
    cws = map (totalWeight m) cs
    result = if length (nub cws) < 2 then (k, w-d) else run m k' d' where
        (k', d') = findDifferent cs cws

findDifferent cs cws = (k, d) where
    g = groupBy (\a b -> (snd a) == (snd b)) $ zip cs cws
    g' = sortBy (\a b -> compare (length a) (length b)) g
    h = nubBy (\a b -> (snd a) == (snd b)) $ concat g'
    k = fst (head h)
    d = (snd (head h)) - (snd (last h))

totalWeight m k = w + (sum $ map (totalWeight m) cs) where
    cs = children m k
    w = weight m k

weight m k = fst $ value m k
children m k = snd $ value m k
value m k = Map.findWithDefault (0,[]) k m

depth m d k = if null children then d else maximum $ map (depth m (d+1)) children where
    (w,children) = Map.findWithDefault (0,[]) k m

parse m (n:w:"->":xs) = m' where
    m' = Map.insert n (readInt w, xs) m
parse m (n:w:[]) = m' where
    m' = Map.insert n (readInt w, []) m

readInt :: String -> Int
readInt = read . filter isNumber
