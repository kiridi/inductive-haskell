module Main(main) where
import Language.Parsing
import Language.FunParser
import Language.Types
import Language.Environment

import Data.Map

import Interpreter
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

-- tenv :: TypeEnv
-- tenv = TypeEnv $ Map.fromList $ [("+", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Int"))
--                                  ()
--                                 ]

-- main = putStr $ show $ runInfer $ infer tenv (Apply (Variable "+") [Number 1, Number 2])