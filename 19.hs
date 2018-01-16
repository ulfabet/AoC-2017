import Data.List
import Data.Maybe
import Data.Char

main = do
    input <- readFile "19.input"
    -- input <- readFile "19a.example"
    print $ day19a input
    print $ day19b input

day19a input = reverse . filter isAlpha $ route m where
    m = lines input

day19b input = length $ route m where
    m = lines input

route m = run m (x,y) (x,y-1) "" where
    (x,y) = findFirst m

run m new old result = if c == ' ' then result else run m new' old' result' where
    c = char m new
    new' = findNext m new old
    old' = new
    result' = c:result

findFirst :: [String] -> (Int, Int)
findFirst m = (fromJust . elemIndex '|' $ head m, 0)

char m (x,y) | x > ((length $ head m)-2) = ' '
char m (x,y) | y > ((length $ m)-2) = ' '
char m (x,y) = (m !! y) !! x

findNext m (x,y) old = new where
    possible = delete old [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]
    new = head $ if isBend m (x,y) then filter (isPipe m) possible else [nextInLine (x,y) old]

isBend m (x,y) = (char m (x,y) == '+')
isPipe m (x,y) = elem (char m (x,y)) (['A'..'Z'] ++ "+-|")

nextInLine (x1,y1) (x0,y0) = (x2,y2) where
    x2 = x1 + (x1-x0)
    y2 = y1 + (y1-y0)

