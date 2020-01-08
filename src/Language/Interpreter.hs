module Language.Interpreter where

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
type EEnv = Environment (Example Value)

data Example a = Pos [a] a
               | Neg [a] a
               deriving Show

data Value =
    IntVal Integer              -- Integers
  | BoolVal Bool                -- Booleans
  | Function ([Value] -> Value) -- Functions
  | Nil                         -- Empty list
  | Cons Value Value            -- Non-empty lists
  | PosExs [Example Value]      -- Positive Example
  | NegExs [Example Value]      -- Negative Example

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

elab (PEx f ins out) env = 
  case maybe_find env ("pos_" ++ f) of
    Nothing           -> define env ("pos_" ++ f) (PosExs [Pos eins eout])
    Just (PosExs exs) -> define env ("pos_" ++ f) (PosExs ((Pos eins eout) : exs))
  where eins = map (\ e -> eval e env) ins
        eout = eval out env

elab (NEx f ins out) env = 
  case maybe_find env ("neg_" ++ f) of
    Nothing           -> define env ("neg_" ++ f) (NegExs [Neg eins eout])
    Just (NegExs exs) -> define env ("neg_" ++ f) (NegExs ((Neg eins eout) : exs))
  where eins = map (\ e -> eval e env) ins
        eout = eval out env

addSignature :: Defn -> TEnv -> TEnv
addSignature (Val name typ _) tenv = define tenv name typ
addSignature (Rec name typ _) tenv = define tenv name typ
addSignature _ tenv = tenv

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

checkTarget :: Ident -> [IFunction] -> VEnv -> Bool
checkTarget target funcs env = 
  if (res_pos get_pex == True && res_neg get_nex == False)
  then True
  else False
  where get_pex = find env ("pos_" ++ target)
        get_nex = find env ("neg_" ++ target)
        func name = 
          case find newEnv name of
            Function f -> Function f
            _ -> error "Target not in the environment"
        checkPosEx (Pos ins out) b = (apply (func target) ins == out) && b
        checkPosEx _ _ = error "Error when checking the positive examples"
        checkNegEx (Neg ins out) b = (apply (func target) ins == out) || b
        checkNegEx _ _ = error "Error when checking the negative examples"
        res_pos (PosExs exs) = foldr checkPosEx True exs
        res_neg (NegExs exs) = foldr checkNegEx False exs
        newEnv = (foldr (\ifun env' -> elab (ifToDefn ifun) env') env (order funcs))
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

obey :: Phrase -> (VEnv, TEnv) -> (String, (VEnv, TEnv))

obey (Calculate exp) (venv,tenv) =
  (print_value (eval exp venv), (venv, tenv))

obey (Define def) (venv,tenv) =
  let x = def_lhs def in
  let venv' = elab def venv in
  let tenv' = addSignature def tenv in
  (if (isEx def) then "" else print_defn venv' x, (venv', tenv'))
  where isEx (PEx _ _ _) = True
        isEx (NEx _ _ _) = True
        isEx _  = False

obey (Synth name typ) (venv, tenv) = (show prog, (venv, tenv))
  where Just (prog, _) = iddfs (check venv) expand initProg
        bkfof = envToList tenv
        myType = find tenv' name
        tenv' = define tenv name typ
        initProg = (IProgram [Incomplete name MEmpty myType []] [] emptyC, (metarules, bkfof, 0))

metarules :: [Metarule]
metarules = [MAP, COMP, FILTER]

check :: VEnv -> (IProgram, State) -> Bool
check env (IProgram [] cs constr, state) = isComplete (IProgram [] cs constr) && 
                                           checkTarget "target" cs env
check _ _ = False

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

  show (PosExs xs) = ""
  show (NegExs xs) = ""