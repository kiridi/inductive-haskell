{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable,
             DeriveTraversable, DeriveGeneric,
             DeriveAnyClass, FlexibleContexts
  #-}

import Data.Functor.Identity
import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Types
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.Map as Map

data TypeF a = BaseType String
             | Fun a a
             | Tuple a a
  deriving (Functor, Foldable, Traversable, Show)


instance Unifiable TypeF where
  zipMatch (BaseType a) (BaseType b) =
    if a == b
      then Just $ BaseType a
      else Nothing
  zipMatch (Fun a1 a2) (Fun b1 b2) =
    Just $ Fun (Right (a1, b1)) (Right (a2, b2))
  zipMatch (Tuple a1 a2) (Tuple b1 b2) =
    Just $ Tuple (Right (a1, b1)) (Right (a2, b2))
  zipMatch _ _ = Nothing

unified :: IntBindingT TypeF Identity
  (Either
    (UFailure TypeF IntVar)
    (UTerm TypeF String))
unified = runExceptT $ do
  a_var <- lift freeVar
  b_var <- lift freeVar
  c_var <- lift freeVar
  d_var <- lift freeVar
  let
    a = UVar a_var
    b = UVar b_var
    c = UVar c_var
    d = UVar d_var
    
    term1 = UTerm (Fun a (UTerm $ Tuple c d))
    term2 = UTerm (Fun c (UTerm $ Tuple a (UTerm $ Fun b a)))

  result <- applyBindings =<< unify term1 term2

  -- replace integer variable identifiers with variable names
  let
    all_vars = Map.fromList
      [(getVarID a_var, "a")
      ,(getVarID b_var, "b")
      ,(getVarID c_var, "c")
      ,(getVarID d_var, "d")
      ]

  return $ fmap ((all_vars Map.!) . getVarID) result

main :: IO ()
main = last [print.runIdentity $ evalIntBindingT unified | i <- [0 .. 10000000]]