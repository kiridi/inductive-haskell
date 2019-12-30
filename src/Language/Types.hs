{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable,
             DeriveTraversable, DeriveGeneric,
             DeriveAnyClass, FlexibleContexts
#-}

module Language.Types (
  HelperTypes
) where

import Data.Functor.Identity
import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Types
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.Map as Map
import GHC.Generics

data HelperTypes a = TInt 
                   | TArray (HelperTypes a)
                   | Arrow [HelperTypes a] (HelperTypes a) 
    deriving (Functor, Foldable, Traversable, Show, Generic1, Unifiable)
