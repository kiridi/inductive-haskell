{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable,
             DeriveTraversable, DeriveGeneric,
             DeriveAnyClass, FlexibleContexts
  #-}

module Types (
    ETypes
) where

import Data.Functor.Identity
import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Types
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.Map as Map
import GHC.Generics

data ETypes a = TInt 
              | TChar 
              | TArray a 
              | TFunc a a
    deriving (Functor, Foldable, Traversable, Show, Generic1, Unifiable)
