module Language.Types where

import Data.Map as Map hiding (foldr)
import Data.Maybe

data HelperType = TVar String
                | BaseType String
                | TArray (HelperType)
                | Arrow [HelperType] (HelperType) 
                deriving (Show, Eq)

type Constraint = Map String HelperType

emptyC :: Constraint
emptyC = Map.empty

unifier :: HelperType -> HelperType -> Constraint -> Maybe Constraint
unifier (BaseType t1) (BaseType t2) const = 
  if t1 == t2
  then Just const
  else Nothing

unifier (Arrow args1 _) (Arrow args2 _) _
  | length args1 /= length args2 = Nothing 

unifier (Arrow args1 res1) (Arrow args2 res2) constr =
  case unifier res1 res2 constr of
    Nothing -> Nothing
    (Just c) -> afterArgsUnif c
  where afterArgsUnif start = foldr unifyArgs (Just start) (zip args1 args2)
        unifyArgs (arg1, arg2) (Just crtConst) = unifier arg1 arg2 crtConst
        unifyArgs _ Nothing = Nothing

unifier (TVar a) (TVar b) const | a == b = Just const

unifier t1 (TArray t2) const | t1 == t2 = Nothing 

unifier (TArray t2) t1 const | t1 == t2 = Nothing 

unifier t (TVar a) const = 
  case Map.lookup a const of
    Just atype -> unifier atype t const
    Nothing -> Just (Map.insert a t const)

unifier (TVar a) t const = 
  case Map.lookup a const of
    Just atype -> unifier atype t const
    Nothing -> Just (Map.insert a t const)

unifier (TArray t1) (TArray t2) constr = unifier t1 t2 constr

unifier _ _ _ = Nothing

applyConstraints :: HelperType -> Constraint -> HelperType -- TODO: Needs verification?
applyConstraints (TVar a) constr = 
  case Map.lookup a constr of
    Just t -> t
    Nothing -> TVar a
applyConstraints (Arrow args res) constr = Arrow (Prelude.map (\a -> applyConstraints a constr) args) (applyConstraints res constr)
applyConstraints (TArray ht) constr = TArray (applyConstraints ht constr)
applyConstraints (BaseType b) _ = BaseType b

freshen :: Int -> HelperType -> HelperType
freshen id (TVar n) = TVar (n ++ show id)
freshen id (TArray t) = TArray (freshen id t)
freshen id (Arrow ins out) = Arrow (Prelude.map (freshen id) ins) (freshen id out)
freshen id (BaseType s) = BaseType s