import Data.List
import Data.List.Split

initialImage = [".#.","..#","###"]

main = do
    -- input <- readFile "21a.example"
    input <- readFile "21.input"
    print $ length . filter (=='#') . concat $ day21a input
    -- print $ day21a input
    -- mapM_ print $ day21a input

day21a input = run 18 initialImage rules where
    rules = map parseLine $ lines input

run 0 image _ = image
run n image rules = run (n-1) (enhance image rules) rules

parseLine line = parseParts $ words line
parseParts [from,"=>",to] = map parsePart [from, to]
parsePart = splitOn "/"

variants image = images where
    images = [ flipx a . flipy b . trans c $ image
        | a <- [False,True], b <- [False,True], c <- [False,True] ]

flipx cond = if cond then map reverse else id
flipy cond = if cond then reverse else id
trans cond = if cond then transpose else id

size image = length $ head image

enhance image rules = combine ((size image)+m) images where
    n = if mod (size image) 2 == 0 then 2 else 3
    m = div (size image) n
    images = map (transform rules) $ squares n image

transform rules image = image' where
    image' = last $ head $ filter (elem image . variants . head) rules

combine :: Int -> [[String]] -> [String]
combine n = map concat . transpose . chunksOf n . concat
    
squares :: Int -> [String] -> [[String]]
squares n = (chunksOf n) . concat . transpose . map (chunksOf n)

-- abcd
-- efgh
-- ijkl
-- mnop
--
-- ["abcd","efgh","ijkl","mnop"]
-- map (chunksOf 2)
-- [["ab","cd"],["ef","gh"],["ij","kl"],["mn","op"]]
-- transpose
-- [["ab","ef","ij","mn"],["cd","gh","kl","op"]]
-- concat 
-- ["ab","ef","ij","mn","cd","gh","kl","op"]
-- chunksOf 2
-- [["ab","ef"],["ij","mn"],["cd","gh"],["kl","op"]]
--

-- divideIntoSquares
-- combineSquares
