module PSBuilder where

import Elements
import Data.List
import Language.Environment
import Language.Types
import Data.Maybe

data IFunction = Incomplete String Metarule HelperType Constraint [FOF]
               | Complete String Metarule HelperType Constraint [FOF] 
               deriving (Eq, Show)
data IProgram = IProgram [IFunction] [IFunction]
               deriving (Eq, Show)

type MetaPool = [Metarule]
type FuncPool = [(String, HelperType)]
type State = (MetaPool, FuncPool)

isComplete :: IProgram -> Bool
isComplete (IProgram [] cs) = all checkComp cs
    where checkComp (Complete _ _ _ _ cs) = True
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
          hasMetarule (IProgram ((Incomplete name MEmpty _ _ _):is) cs) = False
          hasMetarule _ = True

specialize :: IProgram -> Metarule -> FuncPool -> [IProgram]
specialize (IProgram [] cs) mr fp = [IProgram [] cs]
specialize (IProgram (i:is) cs) mr fp = 
    case i of
        Complete _ _ _ _ _ -> [IProgram is (i:cs)]
        Incomplete _ _ _ _ _ -> map createIPs (fill i mr fp []) 
                          where createIPs fun = (IProgram (fun:is) cs)

mrType :: Metarule -> ([HelperType], HelperType)
mrType COMP = ([Arrow [(TVar (TV "b"))] (TVar (TV "c")), 
               Arrow [(TVar (TV "a"))] (TVar (TV "b"))],
               (Arrow [TVar (TV "a")] (TVar (TV "c"))))
mrType MAP = ([Arrow [TVar (TV "a")] (TVar (TV "b"))],
              Arrow [TArray (TVar (TV "a"))] (TArray (TVar (TV "b"))))
mrType FILTER = ([Arrow [TVar (TV "a")] (BaseType "Bool")],
                 Arrow [TArray (TVar (TV "a"))] (TArray (TVar (TV "a"))))

fill :: IFunction -> Metarule -> FuncPool -> [FOF] -> [IFunction]
fill (Incomplete name MEmpty ift _ []) mr fp _ = 
    case newConstraint of
        Just c -> [Incomplete name mr ift c fPhs]
        Nothing -> []
    where (inputs, output) = mrType mr
          newConstraint = unifier ift output emptyC
          fPhs = map (\t -> FEmpty t) inputs

fill (Incomplete name mr ift cons ((FEmpty fType):xs)) _ fp prev = onlyPossible fp
    where onlyPossible [] = []
          onlyPossible ((fn, ft):fs) = 
            case unifier ft fType cons of
                Just newC -> (Incomplete name mr ift newC (prev ++ (FOF fn:xs))):(onlyPossible fs)
                Nothing -> onlyPossible fs

fill (Incomplete name mr fType cons (FOF f:xs)) newMr fp prev = 
    fill (Incomplete name mr fType cons xs) newMr fp (prev ++ [FOF f])
fill (Incomplete name mr fType cons []) _ fp prev  = [Complete name mr fType cons prev] -- is this ok?
fill (Complete name m fTyp cons fofs) _ _ _ = [Complete name m fTyp cons fofs]