module Language.Types where

import Data.Map as Map hiding (foldr)
import Data.Maybe

newtype TVar = TV String
               deriving (Show, Eq, Ord)

data HelperType = TVar TVar
                | BaseType String
                | TArray (HelperType)
                | Arrow [HelperType] (HelperType) 
                deriving (Show, Eq, Ord)

type Constraint = Map TVar (HelperType)

emptyC :: Constraint
emptyC = Map.empty

unifier :: HelperType -> HelperType -> Constraint -> Maybe Constraint

unifier (TVar name1) (TVar name2) const =
  if name1 == name2
  then Just const
  else Nothing

unifier (BaseType t1) (BaseType t2) const = 
  if t1 == t2
  then Just const
  else Nothing

unifier (Arrow args1 res1) (Arrow args2 res2) constr =
  case afterArgsUnif of
    Nothing -> Nothing
    (Just c) -> unifier res1 res2 c
  where afterArgsUnif = foldr unifyArgs (Just constr) (zip args1 args2)
        unifyArgs (arg1, arg2) (Just crtConst) = unifier arg1 arg2 crtConst
        unifyArgs _ Nothing = Nothing

unifier (TVar a) t const = 
  case Map.lookup a const of
    Just atype -> unifier atype t const
    Nothing -> Just (Map.insert a t const)

unifier t (TVar a) const = 
  case Map.lookup a const of
    Just atype -> unifier atype t const
    Nothing -> Just (Map.insert a t const)

unifier (TArray t1) (TArray t2) constr = unifier t1 t2 constr

unifier _ _ _ = Nothing

applyConstraints :: HelperType -> Constraint -> HelperType
applyConstraints (TVar a) constr = fromJust (Map.lookup a constr)
applyConstraints (Arrow args res) constr = Arrow (Prelude.map (\a -> applyConstraints a constr) args) (applyConstraints res constr)
applyConstraints (TArray ht) constr = TArray (applyConstraints ht constr)
applyConstraints (BaseType b) _ = BaseType b