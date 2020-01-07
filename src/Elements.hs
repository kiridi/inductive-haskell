module Elements where
import Language.Types
 
data Metarule = MEmpty
              | IF
              | COMP
              | MAP
              | FOLD
              | FILTER
              deriving (Eq, Ord, Show)

data FOF = FEmpty HelperType
         | FOF String deriving (Eq, Show)
