module Main(main) where
import Language.Parsing
import Language.FunParser
import Language.Types
import Language.Environment

import Data.Map as Map

import Search
import Elements
import Interpreter
import Data.Set as Set
import Data.Maybe

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

-- state :: State
-- state = ([MAP,COMP,FILTER],
--          [("reverse",Arrow [TArray (TVar "a")] (TArray (TVar "a"))),
--           ("reverse1",Arrow [TArray (TVar "a"),TArray (TVar "a")] (TArray (TVar "a"))),
--           ("tl",Arrow [TArray (TVar "a")] (TArray (TVar "a"))),
--           ("1",Arrow [TArray (BaseType "b0")] (TArray (TArray (BaseType "Int")))),
--           ("2",Arrow [TArray (TArray (BaseType "Int"))] (TArray (BaseType "b0")))],
--          Map.fromList [("1",Set.fromList ["1","target"]),("2",Set.fromList ["2","target"]),("target",Set.fromList ["target"])],
--          6)
-- t1 = Arrow [TArray (TArray (BaseType "Int"))] (TArray (TArray (BaseType "Int")))
-- t2 = Arrow [TArray (TVar "b3")] (TArray (TArray (BaseType "Int")))
-- t3 = Arrow [TArray (TArray (BaseType "Int"))] (TArray (TVar "b3"))
-- t4 = Arrow [TArray (BaseType "Int")] (TVar "b3")
-- program :: IProgram
-- program = IProgram
--           [Incomplete "2" MAP t3 [FEmpty t4],
--            Incomplete "1" MEmpty t2 []]
--           [Complete "target" COMP t1 [FOF "1", FOF "2"]]

main = do
    putStrLn "Starting...\n"
    time $ dialog funParser obey (init_env, empty_env, empty_env)

-- sub1 = unify (Arrow [TVar "b5"] (TArray (TArray (BaseType "Int"))))
--              (Arrow [TArray (TVar "a")] (TArray (TVar "a"))) 
             
-- t1 = applySubst (fromJust sub1) $ Arrow [TVar "a1"] (TVar "b1")  
-- t2 = applySubst (fromJust sub1) $ Arrow [TArray (TVar "a2")] (TArray (TVar "b2"))
-- main = putStr $ show $ expand (program, state)