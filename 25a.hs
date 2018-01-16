{-# LANGUAGE BangPatterns #-}

import Data.List
import qualified Data.Map as M

main = do 
    print . checksum $ after 12386363 step ('A',0,M.empty)

checksum (_,_,m) = sum $ M.elems m

-- -- Sadly, this is not strict enough.
-- after n = foldl' (.) id . replicate n

after 0 f s = s
after n f s = after (n-1) f $ seq s f s

-- after 0 f s = s
-- after n f !s = after (n-1) f (f s)

-- after n f s = foldl' f' s [1..n] where
--     f' z n = f z

-- after n f s = foldl' (\z a -> f z) s [1..n]

-- -- Note! Normal iterate is not strict enough.
-- after n f s = (iterate' f s) !! (n+1)

step (state,slot,m) = seq m (state',slot',m') where
    (state',value',dir) = transition (state,getValue)
    m' = M.insert slot value' m
    slot' = slot + dir
    getValue = M.findWithDefault 0 slot m

---- example
-- transition ('A',0) = ('B',1, 1)
-- transition ('A',1) = ('B',0,-1)
-- transition ('B',0) = ('A',1,-1)
-- transition ('B',1) = ('A',1, 1)

---- real
transition ('A',0) = ('B',1, 1)
transition ('A',1) = ('E',0,-1)
transition ('B',0) = ('C',1,-1)
transition ('B',1) = ('A',0, 1)
transition ('C',0) = ('D',1,-1)
transition ('C',1) = ('C',0, 1)
transition ('D',0) = ('E',1,-1)
transition ('D',1) = ('F',0,-1)
transition ('E',0) = ('A',1,-1)
transition ('E',1) = ('C',1,-1)
transition ('F',0) = ('E',1,-1)
transition ('F',1) = ('A',1, 1)
