import Data.List
import qualified Data.Foldable as F

main = do
    let input = 354
    -- let input = 3 -- example
    print $ day17a input
    print $ day17b input
    -- mapM_ putStrLn $ map (show . fst) (test input)

day17a input = take 2 . fst . last . take 2018 $ iterate (run input) ([0], 1)

run steps (xs, y) = (y:(take y . drop (steps+1) $ cycle xs), y+1)

day17b input = xs where 
    xs = foldl' (zot input) (0,0) [1..50000000]

-- zot :: Int -> (Int, Int) -> Int -> (Int, Int)
zot steps (z, v) n | seq steps $ seq z $ seq v $ seq n $ False = undefined
zot steps (z, v) n = (z', v') where
    m = mod (z+(n-steps)) n
    z' = if m == 0 then -1 else m
    v' = if z == steps then n else v

-- bar steps n | seq steps $ seq y $ False = undefined
bar steps 0 y z v = (y,z,v)
bar steps n y z v = bar steps (n-1) (y+1) z v where

-- sim steps (y, z, v) | seq steps $ seq y $ seq z $ seq v $ False = undefined
sim steps (y, z, v) = (y+1, if z' == 0 then -1 else z', v') where
    z' = mod (z+(y-steps)) y
    v' = if z == steps then y else v
