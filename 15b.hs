import Data.Bits

main = do
    let input = (634, 301)
    -- let input = (65, 8921) -- example
    print $ day15b input

day15b (a,b) = length . filter f . take 5000000 $ iterate calc (a,b) where
    f :: (Int,Int) -> Bool
    f (a,b) = (a .&. 0xffff) == (b .&. 0xffff)

calc (a,b) = (calcA a, calcB b)

calcA :: Int -> Int
calcA n = if ((mod x 4) == 0) then x else (calcA x) where
     x = mod (n*16807) 2147483647

calcB :: Int -> Int
calcB n = if ((mod x 8) == 0) then x else (calcB x) where
     x = mod (n*48271) 2147483647


