module Language.Syntax where

import Elements
import Language.Types

data Phrase = Calculate Expr
            | Define Defn
            | Synth Ident HelperType
            deriving Show

data Expr = Number Integer   
          | Variable Ident  
          | Apply Expr [Expr]    
          | If Expr Expr Expr  
          | Lambda [Ident] Expr 
          | Let Defn Expr    
          | Empty          
          | Literal String 
          deriving Show

data Defn = Val Ident HelperType Expr
          | Rec Ident HelperType Expr
          | PEx Ident [Expr] Expr
          | NEx Ident [Expr] Expr
          deriving Show

type Ident = String

def_lhs (Val x _ _) = x
def_lhs (Rec x _ _) = x
def_lhs (PEx f _ _) = "pos_" ++ f
def_lhs (NEx f _ _) = "neg_" ++ f
