module Main(main) where
import Language.Parsing
import Language.FunParser
import Language.Types
import Language.Environment
import Data.Maybe
import qualified Data.Set as Set
import Interpreter
import DepGraph
import Elements
import Search
import Language.Infer
import Language.Types
import Language.Syntax
import qualified Data.Set as Set
import qualified Data.Map as Map

--import Data.Maybe

import Text.Printf
import Control.Exception
import System.CPUTime

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
    putStrLn "Starting...\n"
    time $ dialog funParser obey (init_env, init_tenv, empty_env)

-- tenv :: Environment Scheme
-- tenv = make_env [("+", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Int")),
--                  ("-", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Int"))]

-- fdg = fromJust $ do
--     new1 <- addEdge emptyGraph ("a", "c")
--     new2 <- addEdge new1 ("a", "d")
--     new3 <- addEdge new2 ("c", "d")
--     new4 <- addEdge new3 ("b", "d")
--     new5 <- addEdge new4 ("c", "e")
--     new6 <- addEdge new5 ("f", "e")
--     new7 <- addEdge new6 ("f", "g")
--     new8 <- addEdge new7 ("g", "h")
--     return $ addEdge new8 ("e", "h")

-- compMr = Metarule {
--     name = "comp", 
--     body = Apply (Variable ".") [Hole, Hole],
--     nargs = 2 
-- }
-- mapMr = Metarule {
--     name = "map",
--     body = Apply (Variable "map") [Hole],
--     nargs = 1
-- }
-- metarules = [compMr, mapMr]
-- envenv = make_env [(".", Forall ["a", "b", "c"] $ Arrow (TTuple [Arrow (TTuple [TVar "b"]) (TVar "c"), Arrow (TTuple [TVar "a"]) (TVar "b")]) (Arrow (TTuple [TVar "a"]) (TVar "c"))),
--                 ("map", Forall ["a", "b", "c"] $ Arrow (TTuple [Arrow (TTuple [TVar "a"]) (TVar "b")]) (Arrow (TTuple [TArray (TVar "a")]) (TArray (TVar "b")))),
--                 ("_add1", Forall [] $ Arrow (TTuple [BaseType "Int"]) (BaseType "Int")), 
--                 ("_minus1", Forall [] $ Arrow (TTuple [BaseType "Int"]) (BaseType "Int")),
--                 ("_reverse", Forall ["a"] $ Arrow (TTuple [TArray (TVar "a")]) (TArray (TVar "a"))),
--                 ("_maprev", Forall ["a"] $ Arrow (TTuple [TArray (TArray (TVar "a"))]) (TArray (TArray (TVar "a"))))]

-- iprogram = IProgram {
--     toDoStack = [Val "target" Empty],
--     doneStack = []
-- }

-- pinf = ProgInfo {
--     mrs = metarules,
--     env = envenv,
--     fDepG = fdg,
--     expScheme = Forall ["a1"] $ Arrow (TTuple [TArray (TArray (BaseType "rigid"))]) (TArray (TArray (BaseType "rigid"))),
--     uid = 0
-- }