module Search where

import Elements
import Data.List
import Language.Environment
import Language.Types
import Data.Maybe
import Debug.Trace

type MetaPool = [Metarule]
type Signature = (String, HelperType)
type FuncPool = [Signature]
type State = (MetaPool, FuncPool, Int)

isComplete :: IProgram -> Bool
isComplete (IProgram [] cs) = all checkComp cs
    where checkComp (Complete _ _ _ cs) = True
          checkComp _ = False 
isComplete _ = False

--- IDDFS

selectFirstResult :: (a -> Maybe b) -> [a] -> Maybe b
selectFirstResult select [] = Nothing
selectFirstResult select (x:xs) = 
    case select x of
        Nothing -> selectFirstResult select xs
        _ -> select x 

iddfs :: (a -> Bool) -> (a -> [a]) -> a -> Maybe a
iddfs c e i = selectFirstResult (\d -> (trace ("Searching at depth " ++ show d ++ "...")) boundedSearch d c e i) [1 .. ]
    where boundedSearch d c e crt
            | d == 0         = Nothing
            | c crt == True  = Just crt
            | c crt == False = selectFirstResult (boundedSearch (d - 1) c e) (e crt)

--- for expansion of nodes

expand :: (IProgram, State) -> [(IProgram, State)]
expand (prog, (mp, fp, cnt)) = if hasMetarule prog
                               then
                                   if isComplete prog
                                   then []
                                   else zipWith (\(p, cnt, newFP) m -> (p, (m, newFP, cnt))) (specialize prog MEmpty fp cnt) (repeat mp)
                               else (concat.map create) [0 .. (length mp) - 1]
    where create idx = let progs = specialize prog (mp !! idx) fp cnt in
                       zipWith (\(p, cnt, newFP) m -> (p, (m, newFP, cnt))) progs (repeat mp)
          hasMetarule (IProgram ((Incomplete name MEmpty _ _):is) cs) = False
          hasMetarule _ = True

specialize :: IProgram -> Metarule -> FuncPool -> Int -> [(IProgram, Int, FuncPool)]
specialize (IProgram [] cs) mr fp cnt = [(IProgram [] cs, cnt, fp)]
specialize (IProgram (i:is) cs) mr fp cnt = 
    case i of
        Complete _ _ _ _ -> [(IProgram is (i:cs), cnt, fp)]
        Incomplete _ _ _ _ -> map createIPs (fill i mr fp [] cnt) 
    where createIPs (fun, cnt, subst, newFP) = 
            ((IProgram (map (applyConstIF subst) fun ++ map (applyConstIF subst) is) cs), 
             cnt, 
             map (\(n, t) -> (n, applySubst subst t)) newFP)

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


fill :: IFunction -> Metarule -> FuncPool -> [FOF] -> Int -> [([IFunction], Int, Substitution, FuncPool)]
fill (Incomplete name MEmpty ift []) mr fp _ cnt = 
    case subst of
        Just s -> [([Incomplete name mr ift fPhs], cnt + 1, s, fp)]
        Nothing -> []
    where (inputs, output) = mrType mr cnt
          subst = unify ift output
          fPhs = map (\t -> FEmpty t) inputs

fill (Incomplete name mr ift ((FEmpty fType):xs)) _ fp prev cnt =
    onlyPossible fp ++ 
    [([Incomplete name mr ift (prev ++ (FOF ("gen" ++ show cnt):xs)),
       Incomplete ("gen" ++ show cnt) MEmpty fType []], 
       cnt + 1, emptySubst, fp ++ [("gen" ++ show cnt, fType)])]
    where onlyPossible [] = []
          onlyPossible ((fn, ft):fs) =
            let (corrFT, newCnt) = 
                    if "gen" `isPrefixOf` fn 
                    then (ft, cnt)
                    else (freshen cnt ft, cnt + 1)
            in
            case unify fType corrFT of
                Just subst -> 
                    if "gen" `isPrefixOf` fn && "gen" `isPrefixOf` name 
                    then 
                        if (read (drop 3 name) :: Integer) < (read (drop 3 fn) :: Integer)
                        then ([Incomplete name mr ift (prev ++ (FOF fn:xs))], newCnt, subst, fp):(onlyPossible fs)
                        else (onlyPossible fs)
                    else ([Incomplete name mr ift (prev ++ (FOF fn:xs))], newCnt, subst, fp):(onlyPossible fs)
                Nothing -> onlyPossible fs

fill (Incomplete name mr fType (FOF f:xs)) newMr fp prev cnt = 
    fill (Incomplete name mr fType xs) newMr fp (prev ++ [FOF f]) cnt
fill (Incomplete name mr fType []) _ fp prev cnt = [([Complete name mr fType prev], cnt, emptySubst, fp)]
fill (Complete name m fTyp fofs) _ fp _ cnt = [([Complete name m fTyp fofs], cnt, emptySubst, fp)]