import PSBuilder
import Elements
import Language.Syntax
import Language.Environment
import Language.Interpreter
import Language.Types
import Data.Maybe
import Data.Map

-- Helpers
initFunc = Incomplete "target" MEmpty (intToInt) emptyC []
initProg = IProgram [initFunc] []
intToInt = Arrow [BaseType "Int"] (BaseType "Int")
intToBool = Arrow [BaseType "Int"] (BaseType "Bool")

-- c1 = (fromList [(TV "a",BaseType "Int"),(TV "c",BaseType "Int")])
-- c2 = (fromList [(TV "a",BaseType "Int"),(TV "b",BaseType "Int"),(TV "c",BaseType "Int")])
-- c3 = (fromList [(TV "a",BaseType "Int"),(TV "b",BaseType "Int"),(TV "c",BaseType "Int")])

addOne :: Expr
addOne = Lambda ["x"] (Apply (Variable "+") [Variable "x", Number 1])

addOneEnv :: Env
addOneEnv = thirdEnv
    where firstEnv = define init_env "addOne" (eval addOne init_env)
          secondEnv = elab (PEx "target" [Number 1] (Number 3)) firstEnv
          thirdEnv = elab (NEx "target" [Number 2] (Number 3)) secondEnv

-- Assert function
assert :: Bool -> String -> String -> a -> IO a
assert test passStatement failStatement opt = if test
                                          then do putStrLn passStatement
                                                  return opt
                                          else do putStrLn failStatement
                                                  return opt

testFill :: IO()
testFill = do
           comp <- let opt = fill initFunc COMP [("func", intToInt), ("func2", intToInt)] [] in 
                   assert (opt == [Incomplete "target" COMP intToInt emptyC [FEmpty intToInt, FEmpty intToInt]]) "PASSED: 1" ("FAIL: 1 with " ++ show opt) opt
           comp1 <- let opt = fill (head comp) MEmpty [("func", intToInt), ("func2", intToInt)] [] in
                    assert (opt == [Incomplete "target" COMP intToInt emptyC [FOF "func", FEmpty intToInt]]) "PASSED: 2" ("FAIL: 2 with " ++ show opt) opt
           comp2 <- let opt = fill (head comp1) MEmpty [("func", intToInt), ("func2", intToBool)] [] in
                    assert (opt == [Incomplete "target" COMP intToInt emptyC [FOF "func", FEmpty intToInt]]) "PASSED: 3" ("FAIL: 3 with " ++ show opt) opt
           return ()

-- testSpecialize :: IO()
-- testSpecialize = do
--     prog <- let opt = specialize initProg COMP ["a", "b"] in 
--             assert (opt == [IProgram [Incomplete "target" COMP [FEmpty, FEmpty]] []]) "PASSED: 4" ("FAIL: 4 with " ++ show opt) opt
--     prog1 <- let opt = specialize (head prog) MEmpty ["a", "b"] in
--              assert (opt == [IProgram [Incomplete "target" COMP [FOF "a", FEmpty]] [], IProgram [Incomplete "target" COMP [FOF "b", FEmpty]] []]) "PASSED: 5" ("FAIL: 5 with " ++ show opt) opt
--     prog2 <- let opt = specialize (head prog1) MEmpty ["a", "b"] in
--              assert (opt == [IProgram [Incomplete "target" COMP [FOF "a", FOF "a"]] [], IProgram [Incomplete "target" COMP [FOF "a", FOF "b"]] []]) "PASSED: 6" ("FAIL: 6 with " ++ show opt) opt
--     return ()  

-- testExtend :: IO()
-- testExtend = do
--     progs1 <- let opt = expand (initProg, ([MAP, COMP], ["a", "b"])) in
--         assert (opt == [
--             (IProgram [Incomplete "target" MAP [FEmpty]] [], ([MAP, COMP], ["a", "b"])),
--             (IProgram [Incomplete "target" COMP [FEmpty, FEmpty]] [], ([MAP, COMP], ["a", "b"]))
--         ]) "PASSED: 7" ("FAIL: 7 with " ++ show opt) opt
--     progs2 <- let opt = expand ((fst.head) progs1, ([MAP, COMP], ["a", "b"])) in
--         assert (opt == [
--             (IProgram [Incomplete "target" MAP [FOF "a"]] [], ([MAP, COMP], ["a", "b"])),
--             (IProgram [Incomplete "target" MAP [FOF "b"]] [], ([MAP, COMP], ["a", "b"]))
--         ]) "PASSED: 8" ("FAIL: 8 with " ++ show opt) opt
--     return ()  

-- checkTestSynth :: IO()
-- checkTestSynth = do
--     _ <- let opt = checkSynth "target" [Complete "target" COMP [FOF "addOne", FOF "addOne"]] addOneEnv in
--          assert (opt == True) "PASSED: 9" ("FAIL: 9") opt
--     return ()  

-- checkIDFS :: IO()
-- checkIDFS = do
--     _ <- let opt = iddfs (check addOneEnv) expand (IProgram [Incomplete "target" MEmpty []] [], (metarules, ["addOne"])) in
--          assert ((fst.fromJust) opt == (IProgram [] [Complete "target" COMP [FOF "addOne", FOF "addOne"]])) "PASSED: 10" ("FAIL: 10 with " ++ (show opt)) opt
--     return ()

main :: IO ()
main = do
  putStrLn "Running tests..."
  testFill
--   testSpecialize
--   testExtend
--   checkTestSynth
--   checkIDFS
  putStrLn "done!"
