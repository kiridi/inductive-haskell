module Elements where

import Language.Types
import Data.String
import Data.Char
 
newtype Metarule = Metarule {
    name :: String,
    body :: Expr,
    nargs :: Int
} deriving Show

newtype IFunction = IFunction {
    name :: String,
    mrule :: Metarule,
    holes :: [Type],
    completes :: [String]
}

data IProgram = IProgram {
    toDoStack :: [Defn],
    doneStack :: [Defn]
}

type UniqueID = Int
type Name = String
type Metarules = [Metarule]

data ProgInfo = ProgInfo {
    ip :: IProgram,
    mrs :: Metarules,
    env :: Environment Type,
    fDepG :: FuncDepGraph,
    uid :: UniqueID
}

----- Helpers

isCompleteIP :: IProgram -> Bool
isCompleteIP (IProgram [] cs) = all isCompleteIF cs
isCompleteIP _ = False

addIFtoIP :: IFunction -> IProgram -> IProgram
addIFtoIP ifun (IProgram is cs) = 
    case isCompleteIF ifun of
        True -> IProgram is (ifun:cs)
        False -> IProgram (ifun:is) cs

isCompleteIF :: IFunction -> Bool
isCompleteIF (Complete _ _ _ _) = True
isCompleteIF _ = False

emptyProg :: String -> Type -> IProgram
emptyProg targetName targetType = IProgram [Incomplete targetName MEmpty targetType []] []

hasMetarule :: IProgram -> Bool
hasMetarule (IProgram ((Incomplete _ MEmpty _ _):_) _) = False
hasMetarule _ = True

updateTEnv :: Name -> Type -> ProgInfo -> ProgInfo
updateFContext n t pinf = pinf { fCon = Map.insert n t (fCon pinf) }

updateCounter :: ProgInfo -> ProgInfo
updateCounter pinf = pinf { uid = (uid pinf) + 1 }

popIncomplete :: ProgInfo -> Maybe (IFunction, ProgInfo)
selectIncomplete pinf =
    case pinf ip of 
        IProgram (i:is) cs -> Just (i, pinf { ip = IProgram is cs })
        IProgram [] cs -> Nothing

pushIF :: IFunction -> ProgInfo -> ProgInfo
pushIF ifun state = state { ip = addIFtoIP ifun (ip state) }