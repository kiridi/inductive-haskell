module PSBuilder where

import Elements
-- import Data.Set
-- import Data.PQueue.Max

data IFunction a = New (Metarule a)
                 | Incomplete (Metarule a) [FOF a]
                 | Complete (Metarule a) [FOF a]
data IProgram a = IProgram [IFunction a] [IFunction a]

master :: IProgram -> Int -> IProgram
master prog crt_max = 

idsf :: IProgram a -> Int -> Int -> Maybe IProgram a
idfs crt lvl max = 
    if lvl == max
    then Nothing
    else idfs () (lvl + 1) max

specialize :: IProgram a -> IProgram a
specialize (IProgram [] cs) = (IProgram [] cs)
specialize (IProgram (i:is) cs) = 
    case i of
        Complete _ _ -> (IProgram is (i:cs))
        New _ -> (IProgram ((fill i):is) cs)
        Incomplete _ _ -> (IProgram ((fill i):is) cs)

fill :: IFunction a -> IFunction a
fill (New MEmpty) = Incomplete () []


