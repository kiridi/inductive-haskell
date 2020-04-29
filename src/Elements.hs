module Elements where

import Language.Types
import Language.Syntax
import DepGraph
import Language.Environment
import Data.String
import Data.Char
import Data.List
 
data Metarule = Metarule {
    name :: String,
    body :: Expr,
    nargs :: Int
} deriving Show

data IProgram = IProgram {
    toDoStack :: [Defn],
    doneStack :: [Defn]
}

instance Show IProgram where
    show (IProgram [] doneStack) = "\n" ++ intercalate "\n" (map show doneStack)

type UniqueID = Int
type Metarules = [Metarule]

data ProgInfo = ProgInfo {
    mrs :: Metarules,
    fibk :: [Name],
    fDepG :: DepGraph,
    uid :: UniqueID,
    expType :: Type
} deriving Show

----- Helpers

pushDefn :: Defn -> IProgram -> [Name] -> IProgram
pushDefn defn ip fills = ip {
    doneStack = defn : doneStack ip,
    toDoStack = [Val n Empty | n <- fills] ++ toDoStack ip }
 
popCand :: IProgram -> (Defn, IProgram)
popCand ip = (head (toDoStack ip), ip { toDoStack = tail (toDoStack ip) })

isCompleteIP :: IProgram -> Bool
isCompleteIP ip = null (toDoStack ip)
