import Data.List
import Data.Char

main = do
    -- input <- readFile "18b.example"
    input <- readFile "18.input"
    print $ day18b input

day18b input = (regS m0', regS m1') where
-- day18b input = (m0',m1') where
    m0 = writeReg (0,code,regs,[]) "p" 0
    m1 = writeReg (0,code,regs,[]) "p" 1
    code = map words $ lines input
    regs = replicate 25 0
    (m0', m1') = run m0 m1
    regS m = readReg m "s"

run m0 m1@(pc,code,regs,q) | pc < 0 || pc > ((length code)-1) = (m0, m1)
run m0@(pc,code,regs,q) m1 | pc < 0 || pc > ((length code)-1) = (m0, m1)
run m0 m1 | isDeadlock m0 m1 = (m0, m1)
run m0 m1 = run m0'' m1'' where
    (m0', m1') = step m0 m1
    (m1'', m0'') = step m1' m0'

isDeadlock (pc0,c0,_,q0) (pc1,c1,_,q1) = rcv0 && rcv1 && null q0 && null q1 where
    rcv0 = head (c0 !! pc0) == "rcv"
    rcv1 = head (c1 !! pc1) == "rcv"

step (pc,code,regs,q) n = cmd (pc+1,code,regs,q) n (code !! pc)

regIndex name = (ord $ head name) - ord 'a'

readReg (pc,code,regs,q) name = regs !! (regIndex name)
-- readReg (pc,code,regs,q) name = regs !! ri where
--     ri = if (regIndex name) < 0 then error "regIndex name < 0" else (regIndex name)

writeReg (pc,code,regs,q) name value = (pc,code,regs',q) where
    ri = regIndex name
    regs' = [ if i == ri then value else x | (x,i) <- zip regs [0..] ]

modifyReg m name f = writeReg m name $ f (readReg m name)

-- getValue :: (Int,[[String]],[Int],[Int]) -> String -> Int
getValue m v = if isAlpha (head v) then readReg m v else (read v) :: Int

sendValue m v = (pc,code,regs,v:q) where
    (pc,code,regs,q) = modifyReg m "s" (+1)

receiveValue m@(pc0,code0,regs0,q0) n@(pc1,code1,regs1,q1) name = (m',n') where 
    value = last q1
    q1' = if null q1 then q1 else init q1
    m' = if null q1 then (pc0-1,code0,regs0,q0) else writeReg m name value
    n' = (pc1,code1,regs1,q1')

modifyPC (pc,code,regs,q) f = (f (pc-1),code,regs,q)

cmd m n ["snd",x] = (sendValue m (getValue m x), n)
cmd m n ["set",r,v] = (writeReg m r (getValue m v), n)
cmd m n ["add",r,v] = (modifyReg m r (+(getValue m v)), n)
cmd m n ["mul",r,v] = (modifyReg m r (*(getValue m v)), n)
cmd m n ["mod",r,v] = (modifyReg m r (`mod` (getValue m v)), n)
cmd m n ["rcv",r] = receiveValue m n r
cmd m n ["jgz",r,v] = (if (getValue m r) > 0 then modifyPC m (+(getValue m v)) else m, n)

