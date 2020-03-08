module Elements where

import Language.Types
import Language.Syntax
import DepGraph
import Language.Environment
import Data.String
import Data.Char
 
data Metarule = Metarule {
    name :: String,
    body :: Expr,
    nargs :: Int
} deriving Show

-- newtype IFunction = IFunction {
--     name :: String,
--     mrule :: Metarule,
--     holes :: [Name],
--     completes :: [String]
-- }

data IProgram = IProgram {
    toDoStack :: [Defn],
    doneStack :: [Defn]
}

instance Show IProgram where
    show (IProgram tds ds) = show ds ++ "\n"

type UniqueID = Int
type Metarules = [Metarule]

data ProgInfo = ProgInfo {
    mrs :: Metarules,
    env :: Environment Scheme,
    fDepG :: DepGraph,
    uid :: UniqueID
}

instance Show ProgInfo where
    show _ = ""

----- Helpers

pushDefn :: Defn -> IProgram -> IProgram
pushDefn defn ip = ip {doneStack = defn : doneStack ip}

popCand :: IProgram -> (Defn, IProgram)
popCand ip = (head (toDoStack ip), ip { toDoStack = tail (toDoStack ip) })

-- isCompleteIP :: IProgram -> Bool
-- isCompleteIP (IProgram [] cs) = all isCompleteIF cs
-- isCompleteIP _ = False

-- addIFtoIP :: IFunction -> IProgram -> IProgram
-- addIFtoIP ifun (IProgram is cs) = 
--     case isCompleteIF ifun of
--         True -> IProgram is (ifun:cs)
--         False -> IProgram (ifun:is) cs

-- isCompleteIF :: IFunction -> Bool
-- isCompleteIF (Complete _ _ _ _) = True
-- isCompleteIF _ = False

-- emptyProg :: String -> Type -> IProgram
-- emptyProg targetName targetType = IProgram [Incomplete targetName MEmpty targetType []] []

-- hasMetarule :: IProgram -> Bool
-- hasMetarule (IProgram ((Incomplete _ MEmpty _ _):_) _) = False
-- hasMetarule _ = True

-- updateTEnv :: Name -> Type -> ProgInfo -> ProgInfo
-- updateFContext n t pinf = pinf { fCon = Map.insert n t (fCon pinf) }

-- updateCounter :: ProgInfo -> ProgInfo
-- updateCounter pinf = pinf { uid = (uid pinf) + 1 }

-- popIncomplete :: ProgInfo -> Maybe (IFunction, ProgInfo)
-- selectIncomplete pinf =
--     case pinf ip of 
--         IProgram (i:is) cs -> Just (i, pinf { ip = IProgram is cs })
--         IProgram [] cs -> Nothing

-- pushIF :: IFunction -> ProgInfo -> ProgInfo
-- pushIF ifun state = state { ip = addIFtoIP ifun (ip state) }