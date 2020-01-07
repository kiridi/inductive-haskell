module Main(main) where
import Language.Interpreter
import Language.Parsing
import Language.Syntax
import Language.FunParser
import Language.Environment
import PSBuilder
import Elements
import Data.Set
import Language.Types
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

main = do
    putStrLn "Starting..."
    time $ dialog funParser obey init_env
    putStrLn "Done."