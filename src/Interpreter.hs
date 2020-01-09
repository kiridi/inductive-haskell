module Interpreter where

import Language.Parsing
import Language.Syntax
import Language.FunParser
import Language.Environment
import Language.Types
import Data.Maybe
import Data.List hiding (find)
import Debug.Trace

import Elements
import Search

type VEnv = Environment Value
type TEnv = Environment HelperType
type EEnv = Environment ([Example Value], [Example Value])

data Example a = Pos [a] a
               | Neg [a] a
               deriving Show

data Value =
    IntVal Integer              -- Integers
  | BoolVal Bool                -- Booleans
  | Function ([Value] -> Value) -- Functions
  | Nil                         -- Empty list
  | Cons Value Value            -- Non-empty lists

eval :: Expr -> VEnv -> Value

eval (Number n) env = IntVal n

eval (Variable x) env = find env x

eval (If e1 e2 e3) env =
  case eval e1 env of
    BoolVal True -> eval e2 env
    BoolVal False -> eval e3 env
    _ -> error "boolean required in conditional"

eval (Apply f es) env =
  apply (eval f env) (map ev es)
    where ev e1 = eval e1 env

eval (Lambda xs e1) env = abstract xs e1 env

eval (Let d e1) env = eval e1 (elab d env)

eval e env = error ("can't evaluate")

apply :: Value -> [Value] -> Value
apply (Function f) args = f args
apply _ args = error "applying a non-function"

abstract :: [Ident] -> Expr -> VEnv -> Value
abstract xs e env = 
  Function (\args -> eval e (defargs env xs args))

elab :: Defn -> VEnv -> VEnv
elab (Val x _ e) env = define env x (eval e env)

elab (Rec x _ (Lambda xs e1)) env =
  env' where env' = define env x (abstract xs e1 env')
elab (Rec x _ _) env =
  error "RHS of letrec must be a lambda"

addSignature :: Defn -> TEnv -> TEnv
addSignature (Val name typ _) tenv = define tenv name typ
addSignature (Rec name typ _) tenv = define tenv name typ
addSignature _ tenv = tenv

addExample :: Defn -> VEnv -> EEnv -> EEnv
addExample (PEx name ins out) venv eenv = 
  case maybe_find eenv name of
    Nothing           -> define eenv name ([Pos eins eout], [])
    Just (pos, neg) -> define eenv name ((Pos eins eout) : pos, neg)
  where eins = map (\ e -> eval e venv) ins
        eout = eval out venv

addExample (NEx name ins out) venv eenv = 
  case maybe_find eenv name of
    Nothing           -> define eenv name ([], [Neg eins eout])
    Just (pos, neg) -> define eenv name (pos, (Neg eins eout) : neg)
  where eins = map (\ e -> eval e venv) ins
        eout = eval out venv

ifToDefn :: IFunction -> Defn
ifToDefn (Complete name mr typ fofs) = 
  case mr of 
    COMP -> Val name typ (Lambda ["x"] 
                            (Apply (Variable (getName (fofs!!0))) 
                              [(Apply (Variable (getName (fofs!!1))) 
                                [(Variable "x")])]))
    MAP -> Val name typ (Lambda ["xs"]
                          (Apply (Variable "map") 
                            [Variable (getName (fofs!!0)), Variable "xs"]))
    FILTER -> Val name typ (Lambda ["xs"]
                             (Apply (Variable "filter") 
                               [Variable (getName (fofs!!0)), Variable "xs"]))
  where getName (FOF name) = name
ifToDefn _ = error "Only complete IFunctions should be translated"

checkTarget :: Ident -> [IFunction] -> VEnv-> EEnv -> Bool
checkTarget target funcs venv eenv = 
  if (res_pos get_pex == True && res_neg get_nex == False)
  then True
  else False
  where (get_pex, get_nex) = find eenv target
        func name = 
          case find newEnv name of
            Function f -> Function f
            _ -> error "Target not in the environment"
        checkPosEx (Pos ins out) b = (apply (func target) ins == out) && b
        checkPosEx _ _ = error "Error when checking the positive examples"
        checkNegEx (Neg ins out) b = (apply (func target) ins == out) || b
        checkNegEx _ _ = error "Error when checking the negative examples"
        res_pos exs = foldr checkPosEx True exs
        res_neg exs = foldr checkNegEx False exs
        newEnv = (foldr (\ifun env' -> elab (ifToDefn ifun) env') venv (order funcs))
        order fs = sortBy sf fs
        sf (Complete n1 _ _ _) (Complete n2 _ _ _) = 
          if n1 == "target"
          then LT
          else 
            if n2 == "target"
            then GT
            else if (read (drop 3 n1) :: Integer) < (read (drop 3 n2) :: Integer)
            then LT
            else GT

init_env :: VEnv
init_env = 
  make_env [
    constant "nil" Nil, 
    constant "true" (BoolVal True),
    constant "false" (BoolVal False),
    pureprim "+" (\ [IntVal a, IntVal b] -> IntVal (a + b)),
    pureprim "-" (\ [IntVal a, IntVal b] -> IntVal (a - b)),
    pureprim "*" (\ [IntVal a, IntVal b] -> IntVal (a * b)),
    pureprim "div" (\ [IntVal a, IntVal b] -> 
      if b == 0 then error "Dividing by zero" else IntVal (a `div` b)),
    pureprim "mod" (\ [IntVal a, IntVal b] ->
      if b == 0 then error "Dividing by zero" else IntVal (a `mod` b)),
    pureprim "~" (\ [IntVal a] -> IntVal (- a)),
    pureprim "<" (\ [IntVal a, IntVal b] -> BoolVal (a < b)),
    pureprim "<=" (\ [IntVal a, IntVal b] -> BoolVal (a <= b)),
    pureprim ">" (\ [IntVal a, IntVal b] -> BoolVal (a > b)),
    pureprim ">=" (\ [IntVal a, IntVal b] -> BoolVal (a >= b)),
    pureprim "=" (\ [a, b] -> BoolVal (a == b)),
    pureprim "<>" (\ [a, b] -> BoolVal (a /= b)),
    pureprim "head" (\ [Cons h t] -> h),
    pureprim "tail" (\ [Cons h t] -> t),
    pureprim ":" (\ [a, b] -> Cons a b),
    pureprim "list" (\ xs -> foldr Cons Nil xs),
    pureprim "map" (\[Function f, xs] -> mapply f xs),
    pureprim "filter" (\[Function p, xs] -> fapply p xs)]
    where constant x v = (x, v)
          pureprim x f = (x, Function (primwrap x f))
          
          mapply f Nil = Nil
          mapply f (Cons a b) = Cons (f [a]) (mapply f b)

          fapply p Nil = Nil
          fapply p (Cons a b) = 
            case p [a] of
              BoolVal True -> Cons a (fapply p b)
              BoolVal False -> fapply p b

obey :: Phrase -> (VEnv, TEnv, EEnv) -> (String, (VEnv, TEnv, EEnv))

obey (Calculate exp) (venv, tenv, eenv) =
  (print_value (eval exp venv), (venv, tenv, eenv))

obey (Define def) (venv, tenv, eenv) =
  let x = def_lhs def in
  let venv' = if isEx def then venv else elab def venv in
  let tenv' = if isEx def then tenv else addSignature def tenv in
  let eenv' = if isEx def then addExample def venv eenv else eenv in
  (if (isEx def) then "" else print_defn venv' x, (venv', tenv', eenv'))
  where isEx (PEx _ _ _) = True
        isEx (NEx _ _ _) = True
        isEx _  = False

obey (Synth name typ) (venv, tenv, eenv) = (show prog, (venv, tenv, eenv))
  where Just (prog, _) = iddfs (check venv eenv) expand initProg
        bkfof = envToList tenv
        myType = find tenv' name
        tenv' = define tenv name typ
        initProg = (IProgram [Incomplete name MEmpty myType []] [] emptyC, 
                    (metarules, bkfof, 0))

metarules :: [Metarule]
metarules = [MAP, COMP, FILTER]

check :: VEnv -> EEnv -> (IProgram, State) -> Bool
check venv eenv (IProgram [] cs constr, state) = isComplete (IProgram [] cs constr) && 
                                           checkTarget "target" cs venv eenv
check _ _ _ = False

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = (h1 == h2) && (t1 == t2)
  Function _ == Function _ = error "can't compare functions"
  _ == _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show Nil = "[]"
  show (Cons h t) = "[" ++ show h ++ shtail t ++ "]"
    where 
      shtail Nil = ""
      shtail (Cons h t) = ", " ++ show h ++ shtail t
      shtail x = " . " ++ show x
  show (Function _) = "<function>"