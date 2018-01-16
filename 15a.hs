import Data.Bits

main = do
    let input = (634, 301)
    -- let input = (65, 8921) -- example
    print $ day15a input

day15a (a,b) = length . filter f . take 40000000 $ iterate calc (a,b) where
    f :: (Int,Int) -> Bool
    f (a,b) = (a .&. 0xffff) == (b .&. 0xffff)

calc (a,b) = (mod (a*16807) 2147483647, mod (b*48271) 2147483647)

