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
    