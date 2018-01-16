import Data.List
import qualified Data.Map as M

main = do 
    -- print . checksum $ after 6 step ('A',0,M.empty)
    print . checksum $ after 12386363 step ('A',0,M.empty)

checksum (_,_,m) = sum $ M.elems m

after 0 f s = s
after n f s = after (n-1) f (f s)

step (state,slot,m) | seq state $ seq slot $ seq m $ False = undefined
step (state,slot,m) = (state',slot',m') where
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
