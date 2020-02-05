module Language.Types where

import Data.Map as Map hiding (foldr)
import Data.Maybe
import Debug.Trace

data HelperType = TVar String
                | BaseType String
                | TArray (HelperType)
                | Arrow [HelperType] (HelperType) 
                deriving (Show, Eq)

type Substitution = Map String HelperType

emptySubst :: Substitution
emptySubst = Map.empty

unify :: HelperType -> HelperType -> Maybe Substitution
unify (BaseType t1) (BaseType t2)
  | t1 == t2 = Just emptySubst
  | otherwise = Nothing
unify (Arrow args1 _) (Arrow args2 _)
  | length args1 /= length args2 = Nothing 
unify (Arrow args1 res1) (Arrow args2 res2) =
  Prelude.foldl f (Just emptySubst) zipped
  where zipped = (res1, res2) : zip args1 args2
        f Nothing _ = Nothing
        f (Just s) (t1, t2) = 
          case unify (applySubst s t1) (applySubst s t2) of
            Nothing -> Nothing
            Just nS -> Just (nS `compose` s)
unify t1 (TArray t2) | t1 == t2 = Nothing -- use free variables check for inf types
unify (TArray t2) t1 | t1 == t2 = Nothing
unify t (TVar a) = Just (Map.singleton a t)
unify (TVar a) t = Just (Map.singleton a t)
unify (TArray t1) (TArray t2) = unify t1 t2
unify _ _ = Nothing

applySubst :: Substitution -> HelperType -> HelperType
applySubst subst (TVar a) = 
  case Map.lookup a subst of
    Just t -> t
    Nothing -> TVar a
applySubst subst (Arrow args res) = 
  Arrow (Prelude.map (\a -> applySubst subst a) args) (applySubst subst res)
applySubst subst (TArray ht) = TArray (applySubst subst ht)
applySubst _ (BaseType b) = BaseType b

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = (Map.map (applySubst s1) s2) `Map.union` s1

freshen :: Int -> HelperType -> HelperType
freshen id (TVar n) = TVar (n ++ show id)
freshen id (TArray t) = TArray (freshen id t)
freshen id (Arrow ins out) = Arrow (Prelude.map (freshen id) ins) (freshen id out)
freshen id (BaseType s) = BaseType s
