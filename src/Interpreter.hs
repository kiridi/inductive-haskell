module Interpreter where

import Language.Parsing
import Language.Syntax
import Language.FunParser
import Language.Environment
import Data.Maybe
import Data.Char
import Data.List hiding (find)
import Data.Graph
import DepGraph
import Data.Map as Map hiding (find, foldr, foldl, map, filter)

import Elements
import Language.Types
import Language.Infer
import Search

import Debug.Trace

type VEnv = Environment Value
type EEnv = Environment ([Example Value], [Example Value])

data Example a = Pos [a] a
               | Neg [a] a
               deriving Show

data Value =
    IntVal Integer              -- Integers
  | BoolVal Bool                -- Booleans
  | CharVal Char                -- Characters
  | StringVal String            -- Strings
  | Function ([Value] -> Value) -- Functions
  | Nil                         -- Empty list
  | Cons Value Value            -- Non-empty lists

eval :: Expr -> VEnv -> Value

eval (Number n) env = IntVal n

eval (Character c) env = CharVal c

eval (Variable x) env = find env x

eval (If e1 e2 e3) env =
  case eval e1 env of
    BoolVal True -> eval e2 env
    BoolVal False -> eval e3 env
    _ -> error "boolean required in conditional"

eval (Apply f es) env =
  applyF (eval f env) (map ev es)
    where ev e1 = eval e1 env

eval (Lambda xs e1) env = abstract xs e1 env

eval (Let d e1) env = eval e1 (elab d env)

eval e env = error ("can't evaluate")

applyF :: Value -> [Value] -> Value
applyF (Function f) args = f args
applyF _ args = error "applying a non-function"

abstract :: [Ident] -> Expr -> VEnv -> Value
abstract xs e env = 
  Function (\args -> eval e (defargs env xs args))

elab :: Defn -> VEnv -> VEnv
elab (Val x e) env = define env x (eval e env)

elab (Rec x (Lambda xs e1)) env =
  env' where env' = define env x (abstract xs e1 env')
elab (Rec x _) env =
  error "RHS of letrec must be a lambda"

addSignature :: Defn -> TEnv -> TEnv
addSignature def tenv = 
  case runInfer $ inferDef tenv def of
    Nothing -> error ("Type error when trying to define " ++ (getName def))
    Just sch -> define tenv (getName def) sch

addExample :: Defn -> VEnv -> EEnv -> EEnv
addExample (PEx ins out) venv eenv = 
  case maybe_find eenv "gen0" of
    Nothing -> define eenv "gen0" ([Pos eins eout], [])
    Just (pos, neg) -> define eenv "gen0" ((Pos eins eout) : pos, neg)
  where eins = map (\ e -> eval e venv) ins
        eout = eval out venv

addExample (NEx ins out) venv eenv = 
  case maybe_find eenv "gen0" of
    Nothing         -> define eenv "gen0" ([], [Neg eins eout])
    Just (pos, neg) -> define eenv "gen0" (pos, (Neg eins eout) : neg)
  where eins = map (\ e -> eval e venv) ins
        eout = eval out venv

checkTarget :: [Defn] -> VEnv -> EEnv -> Bool
checkTarget funcs venv eenv =
  res_pos get_pex == True && res_neg get_nex == False
  where (get_pex, get_nex) = find eenv "gen0"
        newEnv = (foldl (\env' def -> elab def env') venv funcs)
        checkPosEx (Pos ins out) b = (applyF (eval (Variable "gen0") newEnv) ins == out) && b
        checkPosEx _ _ = error "Error when checking the positive examples"
        checkNegEx (Neg ins out) b = (applyF (eval (Variable "gen0") newEnv) ins == out) || b
        checkNegEx _ _ = error "Error when checking the negative examples"
        res_pos exs = foldr checkPosEx True exs
        res_neg exs = foldr checkNegEx False exs
        

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
    
    pureprim "isAlpha" (\ [CharVal a] -> BoolVal (isAlpha a)),
    pureprim "isLowerCase" (\ [CharVal a] -> BoolVal (isLower a)),
    pureprim "isUpperCase" (\ [CharVal a] -> BoolVal (isUpper a)),
    pureprim "isDigit" (\ [CharVal a] -> BoolVal (isDigit a)),
    pureprim "list" (\ xs -> foldr Cons Nil xs)]
    -- pureprim "map" (\[Function f, xs] -> mapply f xs),
    -- pureprim "filter" (\[Function p, xs] -> fapply p xs)]
    where constant x v = (x, v)
          pureprim x f = (x, Function (primwrap x f))
          
          -- mapply f Nil = Nil
          -- mapply f (Cons a b) = Cons (f [a]) (mapply f b)

          -- fapply p Nil = Nil
          -- fapply p (Cons a b) = 
          --   case p [a] of
          --     BoolVal True -> Cons a (fapply p b)
          --     BoolVal False -> fapply p b

init_tenv :: TEnv
init_tenv = 
  make_env [
    ("nil", Forall ["a"] $ TArray (TVar "a")), 
    ("true", Forall [] $ BaseType "Bool"),
    ("false", Forall [] $ BaseType "Bool"),
    ("+", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Int")),
    ("-", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Int")),
    ("*", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Int")),
    ("div", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Int")),
    ("mod", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Int")),
    ("~", Forall [] $ Arrow (TTuple [BaseType "Int"]) (BaseType "Int")),
    ("<", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Bool")),
    ("<=", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Bool")),
    (">", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Bool")),
    (">=", Forall [] $ Arrow (TTuple [BaseType "Int", BaseType "Int"]) (BaseType "Bool")),
    ("=", Forall ["a"] $ Arrow (TTuple [TVar "a", TVar "a"]) (BaseType "Bool")),
    ("<>", Forall ["a"] $ Arrow (TTuple [TVar "a", TVar "a"]) (BaseType "Bool")),
    ("head", Forall ["a"] $ Arrow (TTuple [TArray (TVar "a")]) (TVar "a")),
    ("tail", Forall ["a"] $ Arrow (TTuple [TArray (TVar "a")]) (TArray (TVar "a"))),
    (":", Forall ["a"] $ Arrow (TTuple [TVar "a", TArray (TVar "a")]) (TArray (TVar "a"))),
    
    ("isAlpha", Forall [] $ Arrow (TTuple [TArray (BaseType "Char")]) (BaseType "Bool")),
    ("isLower", Forall [] $ Arrow (TTuple [TArray (BaseType "Char")]) (BaseType "Bool")),
    ("isUpper", Forall [] $ Arrow (TTuple [TArray (BaseType "Char")]) (BaseType "Bool")),
    ("isDigit", Forall [] $ Arrow (TTuple [TArray (BaseType "Char")]) (BaseType "Bool")),
    ("isAlpha", Forall [] $ Arrow (TTuple [TArray (BaseType "Char")]) (BaseType "Bool"))]

obey :: Phrase -> (VEnv, TEnv, EEnv) -> (String, (VEnv, TEnv, EEnv))

obey (Calculate exp) (venv, tenv, eenv) =
  case inferExpr tenv exp of
    Nothing -> error "Bad type"
    Just sch -> (print_value (eval exp venv) ++ " :: " ++ show sch, (venv, tenv, eenv))
  
obey (Define def) (venv, tenv, eenv) =
  let x = def_lhs def in
  let venv' = if isEx def then venv else elab def venv in
  let tenv' = if isEx def then tenv else addSignature def tenv in
  let eenv' = if isEx def then addExample def venv eenv else eenv in
  (if (isEx def) then "" else print_defn tenv' x, (venv', tenv', eenv'))
  where isEx (PEx _ _) = True
        isEx (NEx _ _) = True
        isEx _  = False

obey (Synth expectedType) (venv, tenv, eenv) = (show prog, (venv, tenv, eenv))
  where Just prog = progSearch (check venv eenv) expand (initProg, initProgInfo)
        bkfof          = envToList tenv
        initProg       = IProgram [Val "gen0" Empty] [] 
        initProgInfo   = ProgInfo {
          mrs = metarules,
          envI = tenv,
          envG = define empty_env "gen0" (TVar "unknown0"),
          fDepG = emptyGraph,
          uid = 1,
          expType = expectedType
        }

compMr = Metarule {
    name = "comp", 
    body = Apply (Variable "comp") [Hole, Hole],
    nargs = 2 
}
mapMr = Metarule {
    name = "map",
    body = Apply (Variable "map") [Hole],
    nargs = 1
}
metarules = [compMr, mapMr]

check :: VEnv -> EEnv -> (IProgram, ProgInfo) -> Bool
check venv eenv (iprog, pinf) = trace (show iprog) $
  case toDoStack iprog of
    [] -> checkTarget (doneStack iprog) venv eenv
    _  -> False

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  CharVal a == CharVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = (h1 == h2) && (t1 == t2)
  Function _ == Function _ = error "can't compare functions"
  _ == _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show (CharVal c) = show c
  show Nil = "[]"
  show (Cons h t) = "[" ++ show h ++ shtail t ++ "]"
    where 
      shtail Nil = ""
      shtail (Cons h t) = ", " ++ show h ++ shtail t
      shtail x = " . " ++ show x
  show (Function _) = "<function>"