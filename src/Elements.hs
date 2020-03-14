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
    show ip = (show $ toDoStack ip) ++ "\n" ++ (show $ doneStack ip)

type UniqueID = Int
type Metarules = [Metarule]

data ProgInfo = ProgInfo {
    mrs :: Metarules,
    envI :: Environment Scheme,  -- the already infered stuff
    envG :: Environment Type,    -- the to be synth infer stuff
    fDepG :: DepGraph,
    uid :: UniqueID,
    expType :: Type
}

----- Helpers

pushDefn :: Defn -> IProgram -> [Name] -> IProgram
pushDefn defn ip fills = ip {
    doneStack = defn : doneStack ip,
    toDoStack = map (\name -> (Val name Empty)) names ++ toDoStack ip }
    where
        names = filter (\name -> "gen" `isPrefixOf` name) fills

popCand :: IProgram -> (Defn, IProgram)
popCand ip = (head (toDoStack ip), ip { toDoStack = tail (toDoStack ip) })

isCompleteIP :: IProgram -> Bool
isCompleteIP ip = null (toDoStack ip)
