module Language.Types where

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