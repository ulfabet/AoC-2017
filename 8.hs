import Data.List
import qualified Data.Map as M

main = do
    -- input <- readFile "8a.example"
    input <- readFile "8.input"
    print $ day8a input
    print $ day8b input

day8a input = maximum $ M.elems m where
    m = foldl' process M.empty . map words $ lines input

day8b input = max where
    (m,max) = foldl' process (M.empty,0) . map words $ lines input

process (m,max) (r:"inc":v:_:rest) = (m',max') where
    m' = if cond m rest then modifyReg m r (+ (readInt v)) else m
    max' = maximum $ max:M.elems m'
process (m,max) (r:"dec":v:_:rest) = (m',max') where
    m' = if cond m rest then modifyReg m r (flip (-) (readInt v)) else m
    max' = maximum $ max:M.elems m'

cond m [r,">",v] = (readReg m r) > (readInt v)
cond m [r,"<",v] = (readReg m r) < (readInt v)
cond m [r,">=",v] = (readReg m r) >= (readInt v)
cond m [r,"<=",v] = (readReg m r) <= (readInt v)
cond m [r,"==",v] = (readReg m r) == (readInt v)
cond m [r,"!=",v] = (readReg m r) /= (readInt v)

readReg m name = M.findWithDefault 0 name m
writeReg m name value = M.insert name value m
modifyReg m name f = writeReg m name (f $ readReg m name)

readInt :: String -> Int
readInt = read
