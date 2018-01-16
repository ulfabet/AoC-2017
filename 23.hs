import Data.List
import Data.Char

main = do
    -- input <- readFile "23a.example"
    input <- readFile "23.input"
    print $ day23a input

day23a input = run (0,code,regs,0) where
    code = map words $ lines input
    regs = replicate 25 0

-- run :: (Int, [[String]], [Int], Int) -> Int
run (pc,code,regs,count) | pc < 0 || pc > ((length code)-1) = count
run (pc,code,regs,count) = run m' where
    m' = cmd (pc+1,code,regs,count) (code !! pc)

--

regIndex name = (ord $ head name) - ord 'a'

readReg (pc,code,regs,count) name = regs !! (regIndex name)

writeReg (pc,code,regs,count) name value = (pc,code,regs',count) where
    ri = regIndex name
    regs' = [ if i == ri then value else x | (x,i) <- zip regs [0..] ]

modifyReg m name f = writeReg m name $ f (readReg m name)

getValue :: (Int,[[String]],[Int],Int) -> String -> Int
getValue m v = if isAlpha (head v) then readReg m v else read v

modifyPC (pc,code,regs,count) f = (f (pc-1),code,regs,count)

-- cmd m ["snd",x] = playSound m (getValue m x)
cmd m ["set",x,y] = writeReg m x (getValue m y)
cmd m ["sub",x,y] = modifyReg m x (flip (-) (getValue m y))
-- cmd m ["add",r,v] = modifyReg m r (+(getValue m v))
cmd m ["mul",x,y] = (pc,code,regs,count+1) where
    (pc,code,regs,count) = modifyReg m x (*(getValue m y))
-- cmd m ["mod",r,v] = modifyReg m r (`mod` (getValue m v))
-- cmd m ["rcv",r] = if (readReg m r) /= 0 then recoverSound m else m
-- cmd m ["jgz",r,v] = if (readReg m r) > 0 then modifyPC m (+(getValue m v)) else m
cmd m ["jnz",x,y] = if (getValue m x) /= 0 then modifyPC m (+(getValue m y)) else m

