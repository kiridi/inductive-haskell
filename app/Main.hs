module Main(main) where
import Language.Parsing
import Language.FunParser
import Language.Types
import Language.Environment
import Data.Maybe
import Data.List
import Interpreter
import DepGraph
import Elements
import Search
import Language.Infer
import Language.InferSynth
import Language.Types
import Language.Syntax
import qualified Data.Set as Set
import qualified Data.Map as Map

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

fdg = Map.fromList [("gen0",["gen3","gen2"])]

compMr1 = Metarule {
    name = "comp", 
    body = Lambda ["gen_x"] (Apply (Apply (Variable "comp") [Hole, Hole]) [Variable "gen_x"]),
    nargs = 2 
}
mapMr1 = Metarule {
    name = "map",
    body = Lambda ["gen_xs"] (Apply (Apply (Variable "map") [Hole]) [Variable "gen_xs"]),
    nargs = 1
}

metarules1 = [compMr1, mapMr1]
envI' = make_env [("comp", Forall ["a", "b", "c"] $ Arrow (TTuple [Arrow (TTuple [TVar "b"]) (TVar "c"), Arrow (TTuple [TVar "a"]) (TVar "b")]) (Arrow (TTuple [TVar "a"]) (TVar "c"))),
                ("map", Forall ["a", "b"] $ Arrow (TTuple [Arrow (TTuple [TVar "a"]) (TVar "b")]) (Arrow (TTuple [TArray (TVar "a")]) (TArray (TVar "b")))),
                ("BK_reverse", Forall ["a"] $ Arrow (TTuple [TArray (TVar "a")]) (TArray (TVar "a"))),
                ("BK_tail", Forall ["a"] $ Arrow (TTuple [TArray (TVar "a")]) (TArray (TVar "a")))]

envG' = make_env [("gen0", Arrow (TTuple [TVar "d3"]) (TVar "e4")),
                            ("gen2",Arrow (TTuple [TVar "d3"]) (TVar "h7")),
                            ("gen3",Arrow (TTuple [TVar "h7"]) (TVar "e4"))]

iprogram = IProgram {toDoStack = [Val "gen2" Empty,Val "gen3" Empty], doneStack = [Val "gen0" (Lambda ["gen_x"] (Apply (Apply (Variable "comp") [Variable "gen3",Variable "gen2"]) [Variable "gen_x"]))]}

-- IProgram {toDoStack = [Val "gen1" Empty,Val "gen2" Empty], doneStack = [Val "gen0" (Apply (Variable "comp") [Variable "gen2",Variable "gen1"])]}

pinf = ProgInfo {
    mrs = metarules1,
    envG = envG',
    envI = envI',
    fDepG = fdg,
    expType = Arrow (TTuple [TArray (TArray (BaseType "Int"))]) (TArray (TArray (BaseType "Int"))),
    uid = 100
}

-- main = putStr $ intercalate "\n\n" $ map show $ expand (iprogram, pinf)

-- Val "gen5" (Apply (Variable "comp") [Variable "BK_reverse",Variable "BK_reverse"])
-- Val "gen3" (Apply (Variable "map") [Variable "gen5"])
-- Val "gen2" (Apply (Variable "comp") [Variable "gen3",Variable "BK_reverse"])
-- Val "gen0" (Apply (Variable "comp") [Variable "gen3",Variable "gen2"])