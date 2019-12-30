module PSBuilder where

import Elements
import Data.List
import Language.Environment
import Language.Types
import Data.Set hiding (take, map, filter, foldr)
-- import Data.PQueue.Max

data IFunction = Incomplete String Metarule [FOF]
               | Complete String Metarule [FOF] deriving (Eq, Show)
data IProgram = IProgram [IFunction] [IFunction] deriving (Eq, Show)

type MetaPool = [Metarule]
type FuncPool = [String]
type State = (MetaPool, FuncPool)

isComplete :: IProgram -> Bool
isComplete (IProgram [] cs) = all checkComp cs
    where checkComp (Complete _ _ cs) = True
          checkComp _ = False 
isComplete _ = False

--- IDDFS

selectFirstResult :: (a -> Maybe b) -> [a] -> Maybe b
selectFirstResult f [] = Nothing
selectFirstResult f (x:xs) = 
    case f x of
        Nothing -> selectFirstResult f xs
        _ -> f x 

iddfs :: (a -> Bool) -> (a -> [a]) -> a -> Maybe a
iddfs c e i = selectFirstResult (\d -> boundedSearch d c e i) [1 .. ]
    where boundedSearch d c e crt
            | d == 0         = Nothing
            | c crt == True  = Just crt
            | c crt == False = selectFirstResult (boundedSearch (d - 1) c e) (e crt)

--- for expansion of nodes

expand :: (IProgram, State) -> [(IProgram, State)]
expand (prog, (mp, fp)) = if hasMetarule prog 
                          then if isComplete prog 
                               then []
                               else zip (specialize prog MEmpty fp) (repeat (mp, fp))
                          else concat (map create [0 .. (length mp) - 1])
    where create idx = let progs = specialize prog (mp !! idx) fp in
                       zip progs (repeat (mp, fp))
          hasMetarule (IProgram ((Incomplete name MEmpty _):is) cs) = False
          hasMetarule _ = True

specialize :: IProgram -> Metarule -> FuncPool -> [IProgram]
specialize (IProgram [] cs) mr fp = [IProgram [] cs]
specialize (IProgram (i:is) cs) mr fp = 
    case i of
        Complete _ _ _ -> [IProgram is (i:cs)]
        Incomplete _ _ _ -> map createIPs (fill i mr [] fp) 
                          where createIPs fun = (IProgram (fun:is) cs)

nFPh :: Metarule -> Int                                                                                                                                                                                              
nFPh MEmpty = 0
nFPh IF = 3
nFPh COMP = 2
nFPh MAP = 1
nFPh FOLD = 2
nFPh FILTER = 1

fill :: IFunction -> Metarule -> [FOF] -> FuncPool -> [IFunction]
fill (Incomplete name MEmpty []) mr _ fp = [Incomplete name mr fPhs]
    where fPhs = take (nFPh mr) (repeat FEmpty) 
fill (Incomplete name mr (FEmpty:xs)) _  bf fp = map createIFs fp
    where createIFs func = Incomplete name mr (bf ++ (FOF func:xs))
fill (Incomplete name mr (FOF f:xs)) newMr bf fp = fill (Incomplete name mr xs) newMr (bf ++ [FOF f]) fp
fill (Incomplete name mr []) newMr bf fp = [Complete name mr bf] -- is this ok?
fill (Complete name m fofs) mr bf fp = [Complete name m fofs]