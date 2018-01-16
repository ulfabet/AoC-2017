import Data.List
import Data.Char

main = do
    -- input <- readFile "23.input"
    input <- readFile "23b.input"
    print $ day23a input

day23a input = run (0,code,regs,0) where
    code = map words $ lines input
    regs = [1] ++ replicate 7 0

-- run :: (Int, [[String]], [Int], Int) -> Int
run (pc,code,regs,count) | seq pc $ seq code $ seq regs $ seq count $ False = undefined
run (pc,code,regs,count) | pc < 0 || pc > ((length code)-1) = regs
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

cmd m ["set",x,y] = writeReg m x (getValue m y)
cmd m ["sub",x,y] = modifyReg m x (flip (-) (getValue m y))
cmd m ["mul",x,y] = (pc,code,regs,count+1) where
    (pc,code,regs,count) = modifyReg m x (*(getValue m y))
cmd m ["jnz",x,y] = if (getValue m x) /= 0 then modifyPC m (+(getValue m y)) else m

