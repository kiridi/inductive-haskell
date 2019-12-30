module Language.Syntax where

import Data.Char(isAlpha)
import Data.List(intersperse)
import Elements

data Phrase =                 -- Top-level phrase $p$
    Calculate Expr            -- \pg $e$;;
  | Define Defn               -- \pg $d$;;
  | Synth [Char]
  deriving Show

data Expr =                   -- Expressions $e$
    Number Integer            -- $n$
  | Variable Ident            -- $x$
  | Apply Expr [Expr]         -- \pg $e_0$($e_1$, \dots, $e_n$)
  | If Expr Expr Expr         -- \pg if $e_1$ then $e_2$ else $e_3$
  | Lambda [Ident] Expr       -- \pg lambda ($x_1$, \dots, $x_n$) $e$
  | Let Defn Expr             -- \pg let $d$ in $e$
  -- \dots~for records
  | Empty                     -- \pg \{ \}
  -- \dots~for |Fun| in |Fun|
  | Literal String            -- \pg "$s$"
  deriving Show

data Defn =                   -- Definitions $d$
    Val Ident Expr            -- \pg val $x$ = $e$
  | Rec Ident Expr            -- \pg rec $x$ = $e$
  | PEx Ident [Expr] Expr
  | NEx Ident [Expr] Expr
  deriving Show

type Ident = String

def_lhs (Val x e) = x
def_lhs (Rec x e) = x
def_lhs (PEx f _ _) = "pos_" ++ f
def_lhs (NEx f _ _) = "neg_" ++ f

-- |pretty| -- pretty-print an expression with ellipses
pretty :: Expr -> String
pretty exp = pp 8 2 exp ""
  where pp :: Int -> Int -> Expr -> ShowS
        
        pp p d (Number n) = showString (show n)
        
        pp p d (Variable x) = showName x
        
        pp p d (Literal s) =
          showString "\"" . showString s . showString "\""
        
        pp p 0 _ = showString "..."
        
        pp p d (Let def body) =
          showParen (p < 8)
            (showString "let " . pp_def d def 
                . showString " in " . pp 8 (d-1) body)
        
        pp p d (Lambda fps body) =
          showParen (p < 8)
            (showString "lambda (" . pp_list showName fps . showString ") "
                . pp 8 (d-1) body)
        
        pp p d (If e1 e2 e3) =
          showParen (p < 7)
            (showString "if " . pp 7 (d-1) e1 . showString " then " 
                . pp 7 (d-1) e2 . showString " else " . pp 7 (d-1) e3)
        
        pp p d (Apply f aps) = 
          showParen (p < 2)
            (pp 2 d f . showString "(" . pp_list (pp 8 (d-1)) aps . showString ")")
        
        pp p d Empty = showString "{ }"
        
        showName x =
          if isAlpha (head x) then showString x
          else showString "(" . showString x . showString ")"
        
        pp_def :: Int -> Defn -> ShowS
        
        pp_def d (Val x e) =
          showString "val " . showName x . showString " = " . pp 8 (d-1) e
        
        pp_def d (Rec x e) =
          showString "rec " . showName x . showString " = " . pp 8 (d-1) e
        
        pp_list :: (a -> ShowS) -> [a] -> ShowS
        pp_list p = foldr (.) id . intersperse (showString ", ") . map p