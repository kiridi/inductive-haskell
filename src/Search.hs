module Search where

import Elements
import Data.List
import Language.Environment
import Language.Types
import Data.Maybe
import Debug.Trace
import Data.Char
import Data.Map as Map hiding (map, foldr, foldl)
import Data.Set as Set hiding (map, foldr, foldl)
import DepGraph

type MetaPool = [Metarule]
type FuncPool = [(String, HelperType)]
type State = (MetaPool, FuncPool, DepGraph, Int)

--- IDDFS

selectFirstResult :: (a -> Maybe b) -> [a] -> Maybe b
selectFirstResult select [] = Nothing
selectFirstResult select (x:xs) = 
    case select x of
        Nothing -> selectFirstResult select xs
        _       -> select x 

steps :: [Int]
steps = [i | i <- [0 .. ]]

iddfs :: Show a => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
iddfs c e i = selectFirstResult (\d -> (trace ("Searching at depth " ++ show d ++ "...")) boundedSearch d c e i) steps
    where boundedSearch d c e crt
            | d == 0         = Nothing
            | c crt == True  = {-trace (show crt ++ "\n")-} Just crt
            | c crt == False = {-trace (show crt ++ "\n")-} selectFirstResult (boundedSearch (d - 1) c e) (e crt)

--- for expansion of nodes

expand :: (IProgram, State) -> [(IProgram, State)]
expand (prog, (mp, fp, fc, cnt)) 
    | hasMetarule prog = if isCompleteIP prog
                         then []
                         else zipWith (\(p, cnt, newFP, newFC) m -> (p, (m, newFP, newFC, cnt))) (specialize prog MEmpty fp fc cnt) (repeat mp)
    | otherwise        = (concat.map create) [0 .. (length mp) - 1]
    where create idx = let progs = specialize prog (mp !! idx) fp fc cnt in
                       zipWith (\(p, cnt, newFP, newFC) m -> (p, (m, newFP, newFC, cnt))) progs (repeat mp)

specialize :: IProgram -> Metarule -> FuncPool -> DepGraph -> Int -> [(IProgram, Int, FuncPool, DepGraph)]
specialize (IProgram [] cs) mr fp fc cnt = [(IProgram [] cs, cnt, fp, fc)]
specialize (IProgram (i:is) cs) mr fp fc cnt = map createIPs (fill i mr fp fc [] cnt) 
    where createIPs (fun, cnt, subst, newFP, newFC) = 
            ((IProgram (map (applyConstIF subst) (takeIncomp fun) ++ map (applyConstIF subst) is) (takeComp fun ++ cs)), 
             cnt, 
             map (\(n, t) -> (n, applySubst subst t)) newFP,
             newFC)
          takeComp fun = takeWhile isCompleteIF fun
          takeIncomp fun = dropWhile isCompleteIF fun

applyConstIF :: Substitution -> IFunction -> IFunction
applyConstIF subst ifn = 
    case ifn of
        Incomplete name mr t fofs -> 
            Incomplete name mr (applySubst subst t) (applyForFofs fofs subst)
        Complete name mr t fofs ->
            Complete name mr (applySubst subst t) (applyForFofs fofs subst)
    where applyForFofs fofs subst = map (apply subst) fofs
          apply subst (FOF a) = FOF a
          apply subst (FEmpty t) = FEmpty (applySubst subst t)

mrType :: Metarule -> Int -> ([HelperType], HelperType)
mrType COMP cnt = ([Arrow [(TVar ("b" ++ (show cnt)))] (TVar ("c" ++ (show cnt))), 
                    Arrow [(TVar ("a" ++ (show cnt)))] (TVar ("b" ++ (show cnt)))],
                  (Arrow [TVar ("a" ++ (show cnt))] (TVar ("c" ++ (show cnt)))))
mrType MAP cnt = ([Arrow [TVar ("a" ++ (show cnt))] (TVar ("b" ++ (show cnt)))],
                  Arrow [TArray (TVar ("a" ++ (show cnt)))] (TArray (TVar ("b" ++ (show cnt)))))
mrType FILTER cnt = ([Arrow [TVar ("a" ++ (show cnt))] (BaseType "Bool")],
                     Arrow [TArray (TVar ("a" ++ (show cnt)))] (TArray (TVar ("a" ++ (show cnt)))))

fill :: IFunction -> Metarule -> FuncPool -> DepGraph -> [FOF] -> Int -> [([IFunction], Int, Substitution, FuncPool, DepGraph)]
fill (Incomplete name MEmpty ift []) mr fp fc _ cnt = 
    case subst of
        Just s -> [([Incomplete name mr ift fPhs], cnt + 1, s, fp, fc)]
        Nothing -> []
    where (inputs, output) = mrType mr cnt
          subst = unify ift output
          fPhs = map (\t -> FEmpty t) inputs

fill (Incomplete name mr ift ((FEmpty fType):xs)) _ fp fc prev cnt =
    onlyPossible fp ++ 
    [([Incomplete name mr ift (reverse prev ++ (FOF (show cnt):xs)),
       Incomplete (show cnt) MEmpty fType []],
       cnt + 1, emptySubst, fp ++ [(show cnt, fType)], fromJust $ addEdge fc (show cnt, name))]
    where onlyPossible [] = []
          onlyPossible ((fn, ft):fs) = 
            let (corrFT, newCnt) = 
                    if isDigit (head fn) 
                    then (ft, cnt)
                    else (freshen cnt ft, cnt + 1)
            in
            case unify (generalize fType) corrFT of
                Just subst -> 
                    if isDigit (head fn) && isDigit (head name) 
                    then 
                        (case (addEdge fc (fn, name)) of
                            Just newG -> ([Incomplete name mr ift (reverse prev ++ (FOF fn:xs))], newCnt, subst, fp, newG):(onlyPossible fs)
                            Nothing -> onlyPossible fs)
                    else ([Incomplete name mr ift (reverse prev ++ (FOF fn:xs))], newCnt, subst, fp, fc):(onlyPossible fs)
                Nothing -> onlyPossible fs

fill (Incomplete name mr fType (FOF f:xs)) newMr fp fc prev cnt = 
    fill (Incomplete name mr fType xs) newMr fp fc (FOF f : prev) cnt
fill (Incomplete name mr fType []) _ fp fc prev cnt = [([Complete name mr fType (reverse prev)], cnt, emptySubst, fp, fc)]
fill (Complete name m fTyp fofs) _ fp fc _ cnt = [([Complete name m fTyp fofs], cnt, emptySubst, fp, fc)]

generalize (Arrow [TArray x] (TArray y)) = Arrow [x] y
generalize x = x

-- if the metarule is MEmpty, then we can generalize: idea is that
-- if we didnt fill the function we might as well generalize a bit just in case