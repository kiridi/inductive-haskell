module Elements where

import Language.Types
import Data.String
 
data Metarule = MEmpty
              | IF
              | COMP
              | MAP
              | FOLD
              | FILTER

data FOF = FEmpty HelperType
         | FOF String


data IFunction = Incomplete String Metarule HelperType [FOF]
               | Complete String Metarule HelperType [FOF] 

data IProgram = IProgram [IFunction] [IFunction] Constraint

instance Show FOF where
    show (FEmpty _) = "???"
    show (FOF name) = name

instance Show IFunction where
    show (Incomplete _ _ _ _) = "???"
    show (Complete name mr _ fofs) = name ++ " = " ++ 
        case mr of
            MEmpty -> "???"
            IF -> "if " ++ show (fofs!!0) ++ " else " ++ show (fofs!!1) ++ "then" ++ show (fofs!!2)
            COMP -> show (fofs!!0) ++ "." ++ show (fofs!!1)
            MAP -> "map " ++ show (fofs!!0)
            FOLD -> "fold " ++ show (fofs!!0)
            FILTER -> "filter " ++ show (fofs!!0)

instance Show IProgram where
    show (IProgram _ cs _) = foldr addNl "" ((map show.reverse) cs)
        where addNl c str = c ++ "\n" ++ str
