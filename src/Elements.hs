module Elements where
import Language.Types

-- data Ph a = MPh (Metarule a)
--           | FPh Int (FOF a)
 
data Metarule = MEmpty
              | IF
              | COMP
              | MAP
              | FOLD
              | FILTER deriving (Eq, Ord, Show)

data FOF = FEmpty
         | FOF String deriving (Eq, Show)
