module Elements where
import Language.Types

-- data Ph a = MPh (Metarule a)
--           | FPh Int (FOF a)
 
data Metarule a = MEmpty
                | IF a a a
                | COMP a a
                | MAP a
                | FOLD a
                | FILTER a

data FOF a = FEmpty
           | FOF Value

initTarget :: Metarule a
initTarget = MEmpty

