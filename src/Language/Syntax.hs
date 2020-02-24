module Language.Syntax where

import Language.Types

data Phrase = Calculate Expr
            | Define Defn
            | Synth Ident Type
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
          deriving Show

data Defn = Val Ident Expr
          | Rec Ident Expr
          | PEx Ident [Expr] Expr
          | NEx Ident [Expr] Expr
          deriving Show

type Ident = String

def_lhs (Val x _) = x
def_lhs (Rec x _) = x
def_lhs (PEx f _ _) = "pos_" ++ f
def_lhs (NEx f _ _) = "neg_" ++ f

getName (Val n _) = n
getName (Rec n _) = n
