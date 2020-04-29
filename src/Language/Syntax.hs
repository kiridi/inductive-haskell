module Language.Syntax where

import Language.Types
import Data.String
import Data.Char
import Data.List

data Phrase = Calculate Expr
            | Define Defn
            | Synth Type
            deriving Show

data Expr = Number Integer
          | Character Char
          | Variable Ident  
          | Apply Expr [Expr]
          | If Expr Expr Expr
          | Lambda [Ident] Expr
          | Let Defn Expr
          | Hole
          | Empty

instance Show Expr where
    show (Variable n) = n
    show (Apply e2 es) = show e2 ++ " " ++ intercalate " " (map show es)

data Defn = Val Ident Expr
          | Rec Ident Expr
          | PEx [Expr] Expr
          | NEx [Expr] Expr
          
instance Show Defn where
    show (Val id exp) = id ++ " = " ++ show exp

type Ident = String

def_lhs (Val x _) = x
def_lhs (Rec x _) = x
def_lhs (PEx _ _) = "pos"
def_lhs (NEx _ _) = "neg"

getName (Val n _) = n
getName (Rec n _) = n
getBody (Val _ b) = b
getBody (Rec _ b) = b
