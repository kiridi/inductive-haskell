module Language.Interpreter where

import Language.Parsing
import Language.Syntax
import Language.FunParser
import Language.Environment
import Language.Types
import Data.Maybe
import Control.Exception

import Elements
import PSBuilder

-- An environment is a map from identifiers to values
type Env = Environment Value

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
  | Success String              -- Successfully synthesized
  | Failure String              -- Successfully synthesized
  | Excp

eval :: Expr -> Env -> Value

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

eval e env =
  error ("can't evaluate " ++ pretty e)

apply :: Value -> [Value] -> Value
apply (Function f) args = f args
apply _ args = error "applying a non-function"

abstract :: [Ident] -> Expr -> Env -> Value
abstract xs e env = 
  Function (\args -> eval e (defargs env xs args))

elab :: Defn -> Env -> Env
elab (Val x e) env = define env x (eval e env)

elab (Rec x (Lambda xs e1)) env =
  env' where env' = define env x (abstract xs e1 env')
elab (Rec x _) env =
  error "RHS of letrec must be a lambda"

elab (PEx f ins out) env = 
  case env_ex of
    Nothing           -> define env ("pos_" ++ f) (PosExs [Pos eins eout])
    Just (PosExs exs) -> define env ("pos_" ++ f) (PosExs ((Pos eins eout) : exs))
  where env_ex = maybe_find env ("pos_" ++ f)
        eins = map (\ e -> eval e env) ins
        eout = eval out env

elab (NEx f ins out) env = 
  case env_ex of
    Nothing           -> define env ("neg_" ++ f) (NegExs [Neg eins eout])
    Just (NegExs exs) -> define env ("neg_" ++ f) (NegExs ((Neg eins eout) : exs))
  where env_ex = maybe_find env ("neg_" ++ f)
        eins = map (\ e -> eval e env) ins
        eout = eval out env

synthTranslateDefn :: IFunction -> Defn
synthTranslateDefn (Complete name mr fofs) = 
  case mr of 
    COMP -> Val name (Lambda ["x"] 
                             (Apply (Variable (extractName (fofs!!0))) 
                                    [(Apply (Variable (extractName (fofs!!1))) 
                                            [(Variable "x")])]))
    MAP -> Val name (Lambda ["xs"]
                            (Apply (Variable "map") [Variable (extractName (fofs!!0)), Variable "xs"]))

  where extractName (FOF name) = name
synthTranslateDefn _ = error "should be complete"

checkSynth :: Ident -> [IFunction] -> Env -> Bool
checkSynth target funcs env = 
  if (res_pos get_pex == True && res_neg get_nex == False)
  then True --(Success target)
  else False --(Failure target)
  where get_pex = find env ("pos_" ++ target)
        get_nex = find env ("neg_" ++ target)
        func name = 
          case find newEnv name of
            Function f -> Function f
            _ -> error "wtf f"
        f (Pos ins out) b = (apply (func target) ins == out) && b
        f _ _ = error "wtf p"
        g (Neg ins out) b = (apply (func target) ins == out) || b
        g _ _ = error "wtf n"
        res_pos (PosExs exs) = foldr f True exs
        res_neg (NegExs exs) = foldr g False exs
        newEnv = foldr (\ifun env' -> elab (synthTranslateDefn ifun) env') env funcs

init_env :: Env
init_env = 
  make_env [constant "nil" Nil, 
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
    pureprim "integer" (\ [a] ->
      case a of IntVal _ -> BoolVal True; _ -> BoolVal False),
    pureprim "head" (\ [Cons h t] -> h),
    pureprim "tail" (\ [Cons h t] -> t),
    pureprim ":" (\ [a, b] -> Cons a b),

    pureprim "map" (\[Function f, xs] -> mapply f xs)]
    where constant x v = (x, v)
          pureprim x f = (x, Function (primwrap x f))
          mapply f (Cons a b) = Cons (f [a]) (mapply f b)
          mapply f Nil = Nil

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

  show (PosExs xs) = "" -- show (map show xs)
  show (NegExs xs) = "" -- show (map show xs)
  show (Success name) = name ++ " successfully synthesized"
  show (Failure name) = name ++ " not synthesized"

obey :: Phrase -> Env -> (String, Env)

obey (Calculate exp) env =
  (print_value (eval exp env), env)

obey (Define def) env =
  let x = def_lhs def in
  let env' = elab def env in
  (if (ex_def def) then "NoPrint" else print_defn env' x, env')
  where ex_def (PEx _ _ _) = True
        ex_def (NEx _ _ _) = True
        ex_def _  = False

obey (Synth name) env = (name ++ " added to env: " ++ show (extractTarget comps), newEnv)
  where Just ((IProgram _ comps), _) = iddfs (check env) expand (IProgram [Incomplete name MEmpty []] [], (metarules, ["addOne"]))
        newEnv = elab (synthTranslateDefn $ extractTarget comps) env
        extractTarget [] = error "???"
        extractTarget ((Complete fn x y):fs) =
          if fn == name
          then (Complete fn x y)
          else extractTarget fs
metarules :: [Metarule]
metarules = [MAP]

check :: Env -> (IProgram, State) -> Bool
check env (IProgram [] cs, state) = isComplete (IProgram [] cs) && checkSynth "target" cs env
check _ _ = False
