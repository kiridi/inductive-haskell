module Elements where

import Language.Types
import Data.String
import Data.Char
 
data Metarule = MEmpty
              | IF
              | COMP
              | MAP
              | FOLD
              | FILTER deriving Show

data FOF = FEmpty Type
         | FOF String

-- TODO: break [FOF] in 2 lists, one for empty holes, one for full holes
data IFunction = Incomplete String Metarule Type [FOF]
               | Complete String Metarule Type [FOF]

data IProgram = IProgram [IFunction] [IFunction]

instance Show FOF where
    show (FEmpty t) = "FEmpty " ++ show t
    show (FOF name) = 
        (if isDigit (head name) then "gen" else "") ++ name

instance Show IFunction where
    show (Incomplete name mr t fofs) = 
        (if isDigit (head name) then "gen" else "") ++ name ++ " = " ++ 
        case mr of
            MEmpty -> "Empty function, type:" ++ show t
            IF -> "if " ++ show (fofs!!0) ++ " else " ++ show (fofs!!1) ++ "then" ++ show (fofs!!2)
            COMP -> show (fofs!!0) ++ "." ++ show (fofs!!1)
            MAP -> "map " ++ show (fofs!!0)
            FOLD -> "fold " ++ show (fofs!!0)
            FILTER -> "filter " ++ show (fofs!!0)
    show (Complete name mr t fofs) = 
        (show t) ++ "\n" ++ (if isDigit (head name) then "gen" else "") ++ name ++ " = " ++ 
        case mr of
            MEmpty -> "Empty function, type" ++ show t
            IF -> "if " ++ show (fofs!!0) ++ " else " ++ show (fofs!!1) ++ "then" ++ show (fofs!!2)
            COMP -> show (fofs!!0) ++ "." ++ show (fofs!!1)
            MAP -> "map " ++ show (fofs!!0)
            FOLD -> "fold " ++ show (fofs!!0)
            FILTER -> "filter " ++ show (fofs!!0)
 
instance Show IProgram where
    show (IProgram is cs) = (foldr addNl "" ((map show.reverse) cs)) ++ (foldr addNl "" ((map show.reverse) is)) ++ "\n----\n"
        where addNl c str = c ++ "\n" ++ str

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
