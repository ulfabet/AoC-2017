import Data.List
import Data.List.Split

initialImage = [".#.","..#","###"]

main = do
    input <- readFile "21.input"
    let rules = map parseLine $ lines input
    print . countPixels $ after 5 iterations where
        after n f = f !! n
        iterations = iterate enhance initialImage
        enhance image = joinImages . map convert $ splitImage image
        convert image = snd . head $ filter (elem image . fst) rules

countPixels = length . filter (=='#') . concat 

parseLine = parseParts . words
parseParts [from,"=>",to] = (variants $ parsePart from, parsePart to)
parsePart = splitOn "/"

variants image = images where
    images = [ flipx a . flipy b . trans c $ image
        | a <- [False,True], b <- [False,True], c <- [False,True] ]
    flipx cond = if cond then map reverse else id
    flipy cond = if cond then reverse else id
    trans cond = if cond then transpose else id

joinImages :: [[String]] -> [String]
joinImages images = map concat . transpose . chunksOf n $ concat images where
    n = div (length $ concat images) 2 

splitImage :: [String] -> [[String]]
splitImage image = chunksOf n . concat . transpose $ map (chunksOf n) image where
    n = if mod (length image) 2 == 0 then 2 else 3

