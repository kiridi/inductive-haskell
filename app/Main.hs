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

main = dialog funParser obey init_env
