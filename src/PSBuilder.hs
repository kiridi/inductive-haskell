module PSBuilder where

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
isComplete (IProgram [] cs _) = all checkComp cs
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
iddfs c e i = selectFirstResult (\d -> (trace ("Searching at depth" ++ show d ++ "...")) boundedSearch d c e i) [1 .. ]
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
          hasMetarule (IProgram ((Incomplete name MEmpty _ _):is) cs _) = False
          hasMetarule _ = True

specialize :: IProgram -> Metarule -> FuncPool -> Int -> [(IProgram, Int, FuncPool)]
specialize (IProgram [] cs cons) mr fp cnt = [(IProgram [] cs cons, cnt, fp)]
specialize (IProgram (i:is) cs cons) mr fp cnt = 
    case i of
        Complete _ _ _ _ -> [(IProgram is (i:cs) cons, cnt, fp)]
        Incomplete _ _ _ _ -> map createIPs (fill i mr fp [] cnt cons) 
    where createIPs (fun, cnt, newCons, newFP) = 
            ((IProgram (fun ++ is) (cs) newCons), cnt, newFP)
          newIs fun = filter (\f -> (not.isCF) f) fun
          newCs fun = filter (\f -> isCF f) fun
          isCF f = 
              case f of 
                  Complete _ _ _ _ -> True
                  Incomplete _ _ _ _ -> False

mrType :: Metarule -> Int -> ([HelperType], HelperType)
mrType COMP cnt = ([Arrow [(TVar ("b" ++ (show cnt)))] (TVar ("c" ++ (show cnt))), 
                    Arrow [(TVar ("a" ++ (show cnt)))] (TVar ("b" ++ (show cnt)))],
                  (Arrow [TVar ("a" ++ (show cnt))] (TVar ("c" ++ (show cnt)))))
mrType MAP cnt = ([Arrow [TVar ("a" ++ (show cnt))] (TVar ("b" ++ (show cnt)))],
                  Arrow [TArray (TVar ("a" ++ (show cnt)))] (TArray (TVar ("b" ++ (show cnt)))))
mrType FILTER cnt = ([Arrow [TVar ("a" ++ (show cnt))] (BaseType "Bool")],
                     Arrow [TArray (TVar ("a" ++ (show cnt)))] (TArray (TVar ("a" ++ (show cnt)))))

fill :: IFunction -> Metarule -> FuncPool -> [FOF] -> Int -> Constraint -> [([IFunction], Int, Constraint, FuncPool)]
fill (Incomplete name MEmpty ift []) mr fp _ cnt cons = 
    case newConstraint of
        Just c -> [([Incomplete name mr ift fPhs], cnt + 1, c, fp)]
        Nothing -> []
    where (inputs, output) = mrType mr cnt
          newConstraint = unifier ift output cons
          fPhs = map (\t -> FEmpty t) inputs

fill (Incomplete name mr ift ((FEmpty fType):xs)) _ fp prev cnt cons =
    onlyPossible fp ++ 
    [([Incomplete name mr ift (prev ++ (FOF ("gen" ++ show cnt):xs)),
       Incomplete ("gen" ++ show cnt) MEmpty fType []], cnt + 1, cons, fp ++ [("gen" ++ show cnt, fType)])]
    where onlyPossible [] = []
          onlyPossible ((fn, ft):fs) =
            let (corrFT, newCnt) = if "gen" `isPrefixOf` fn 
                         then (ft, cnt)
                         else (freshen cnt ft, cnt + 1) 
            in
            case unifier fType corrFT cons of
                Just newC -> 
                    if "gen" `isPrefixOf` fn && "gen" `isPrefixOf` name 
                    then 
                        if (read (drop 3 name) :: Integer) < (read (drop 3 fn) :: Integer)
                        then ([Incomplete name mr ift (prev ++ (FOF fn:xs))], newCnt, newC, fp):(onlyPossible fs)
                        else (onlyPossible fs)
                    else ([Incomplete name mr ift (prev ++ (FOF fn:xs))], newCnt, newC, fp):(onlyPossible fs)
                Nothing -> onlyPossible fs

fill (Incomplete name mr fType (FOF f:xs)) newMr fp prev cnt cons = 
    fill (Incomplete name mr fType xs) newMr fp (prev ++ [FOF f]) cnt cons
fill (Incomplete name mr fType []) _ fp prev cnt cons = [([Complete name mr fType prev], cnt, cons, fp)] -- is this ok?
fill (Complete name m fTyp fofs) _ fp _ cnt cons = [([Complete name m fTyp fofs], cnt, cons, fp)]