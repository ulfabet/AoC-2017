import Data.List
import Data.List.Split
import qualified Data.Map as M

main = do
    input <- readFile "22.input"
    -- input <- readFile "22a.example"
    print $ day22a input
    print $ day22b input

day22a input = infectedCount $ after 10000 (iterations m) where
    m = foldl' parseRow M.empty $ zip (lines input) [0..]
    parseRow m (line,y) = m' where
        m' = foldl' parseCol m $ zip line [0..]
        parseCol m (char,x) = M.insert (x,y) char m

day22b input = infectedCount $ after 10000000 m where
    m = foldl' parseRow M.empty $ zip (lines input) [0..]
    parseRow m (line,y) = m' where
        m' = foldl' parseCol m $ zip line [0..]
        parseCol m (char,x) = M.insert (x,y) char m

infectedCount (_,_,i) = i
after n m = foldl' burst (m,(12,12,0),0) [1..n]

burst (m,(x,y,d),i) _ | seq m $ seq x $ seq y $ seq d $ seq i $ False = undefined
burst (m,(x,y,d),i) _ = (m',(x',y',d'),i') where
    c = M.findWithDefault '.' (x,y) m
    m' = M.insert (x,y) (change c) m
    d' = mod (d + (turn c)) 4
    [x',y'] = zipWith (+) [x,y] $ [[0,-1],[1,0],[0,1],[-1,0]] !! d'
    i' = if c == 'W' then i+1 else i

change '.' = 'W'
change 'W' = '#'
change '#' = 'F'
change 'F' = '.'

turn '.' = -1
turn 'W' = 0
turn '#' = 1
turn 'F' = 2

