module Language.Types where

import Data.List

type Name = String

data Type = TVar Name
          | BaseType Name
          | TArray Type
          | TTuple [Type]
          | Arrow Type Type -- first type MUST be a tuple
          deriving Eq
data Scheme = Forall [Name] Type

instance Show Type where
    show (TVar n) = n
    show (BaseType n) = n
    show (TArray t) = "[" ++ show t ++ "]"
    show (TTuple ts) = "(" ++ intercalate "," (map show ts) ++ ")"
    show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2

instance Show Scheme where
    show (Forall fvs t) = "forall " ++ concat fvs ++ " . " ++ show t