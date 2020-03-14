module Language.Syntax where

import Language.Types

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
          deriving Show

data Defn = Val Ident Expr
          | Rec Ident Expr
          | PEx [Expr] Expr
          | NEx [Expr] Expr
          deriving Show

type Ident = String

def_lhs (Val x _) = x
def_lhs (Rec x _) = x
def_lhs (PEx _ _) = "pos"
def_lhs (NEx _ _) = "neg"

getName (Val n _) = n
getName (Rec n _) = n
getBody (Val _ b) = b
getBody (Rec _ b) = b
