module Language.Syntax where

import Elements
import Language.Types

data Phrase =
    Calculate Expr
  | Define Defn
  | Synth [Char]
  deriving Show

data Expr =
    Number Integer   
  | Variable Ident  
  | Apply Expr [Expr]    
  | If Expr Expr Expr  
  | Lambda [Ident] Expr 
  | Let Defn Expr    
  | Empty          
  | Literal String 
  deriving Show

data Defn =
    Val Ident Expr
  | Rec Ident Expr
  | PEx Ident [Expr] Expr
  | NEx Ident [Expr] Expr
  deriving Show

type Ident = String

def_lhs (Val x e) = x
def_lhs (Rec x e) = x
def_lhs (PEx f _ _) = "pos_" ++ f
def_lhs (NEx f _ _) = "neg_" ++ f
