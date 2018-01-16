import Data.List
import Data.Char

main = do
    -- input <- readFile "18a.example"
    input <- readFile "18.input"
    print $ day18a input

day18a input = run (0,code,regs,[]) where
    code = map words $ lines input
    regs = replicate 25 0

run (pc,code,regs,sounds) | (length sounds) > 0 = sounds
run (pc,code,regs,sounds) | pc < 0 || pc > ((length code)-1) = sounds
run (pc,code,regs,sounds) = run m' where
    m' = cmd (pc+1,code,regs,sounds) (code !! pc)

regIndex name = (ord $ head name) - ord 'a'

readReg (pc,code,regs,sounds) name = regs !! (regIndex name)

writeReg (pc,code,regs,sounds) name value = (pc,code,regs',sounds) where
    ri = regIndex name
    regs' = [ if i == ri then value else x | (x,i) <- zip regs [0..] ]

modifyReg m name f = writeReg m name $ f (readReg m name)

getValue :: (Int,[[String]],[Int],[Int]) -> String -> Int
getValue m v = if isAlpha (head v) then readReg m v else read v

playSound m v = writeReg m "s" v
recoverSound m@(pc,code,regs,sounds) = (pc,code,regs,(readReg m "s"):sounds)

modifyPC (pc,code,regs,sounds) f = (f (pc-1),code,regs,sounds)

cmd m ["snd",x] = playSound m (getValue m x)
cmd m ["set",r,v] = writeReg m r (getValue m v)
cmd m ["add",r,v] = modifyReg m r (+(getValue m v))
cmd m ["mul",r,v] = modifyReg m r (*(getValue m v))
cmd m ["mod",r,v] = modifyReg m r (`mod` (getValue m v))
cmd m ["rcv",r] = if (readReg m r) /= 0 then recoverSound m else m
cmd m ["jgz",r,v] = if (readReg m r) > 0 then modifyPC m (+(getValue m v)) else m

