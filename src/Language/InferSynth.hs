{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InferSynth where

import Prelude hiding (foldr)

import Language.Types
import Language.Syntax
import Language.Environment
import Language.Infer

import Elements

import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Monoid
import Data.List (nub, isPrefixOf)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

runStepInfer :: Int -> Infer (Subst, Type, Maybe [Name], Expr) -> Maybe (Subst, Type, Expr, Int)
runStepInfer cnt m = case runState (runMaybeT m) (Unique { count = cnt }) of
  (Nothing, _) -> Nothing
  (Just (s, t, _, newE), unq) -> Just $ (s, t, newE, count unq + 1)

simpleLookup :: Environment Type -> Name -> Type
simpleLookup env name = find env name

freshenEnv :: Int -> Environment Type -> Environment Type
freshenEnv n env = envMap env (freshenTyp n)

freshenTyp :: Int -> Type -> Type
freshenTyp n (TVar v) = TVar (show n ++ '_':v)
freshenTyp n (TArray t) = TArray (freshenTyp n t)
freshenTyp n (Arrow ins out) = Arrow (freshenTyp n ins) (freshenTyp n out)
freshenTyp n (TTuple ts) = TTuple (map (freshenTyp n) ts)
freshenTyp n (BaseType s) = BaseType s

getFirstHole :: Maybe [Name] -> (Name, Maybe [Name])
getFirstHole maybeH = 
  case maybeH of
    Nothing -> error "Should not be getting holes"
    Just holes -> if holes == [] 
                  then error "Insuficient holes"
                  else (head holes, Just (tail holes))

canUnify :: Type -> Type -> Maybe Subst
canUnify t1 t2 = evalState (runMaybeT $ unify t1 t2) initUnique

-- * The resoning for the `fills` argument is that we might want to infer some 
--   amount of holes (or none), so we fill holes in order, from left to right. 
-- * envI is immutable, envG will be mutable since this is where we add temporary
--   type variables and new definition's types.
inferSynth :: TEnv -> Environment Type -> Expr -> Maybe [Name] -> Infer (Subst, Type, Maybe [Name], Expr)
inferSynth envI envG ex fills = case ex of

  Number n -> returnInfer (nullSubst, BaseType "Int", fills, Number n)

  Character c -> returnInfer (nullSubst, BaseType "Char", fills, Character c)

  Variable v -> do
    t <- if ("gen" `isPrefixOf` v) then return (simpleLookup envG v) else lookupAndInst envI v
    return (nullSubst, t, fills, Variable v)

  Lambda vs expr -> do
    tvs <- replicateM (length vs) fresh
    let envG' = foldl (\ te (tv, v) -> define te v tv) envG (zip tvs vs)
    (s, t, fs, newE) <- inferSynth envI envG' expr fills
    return (s, Arrow (apply s (TTuple tvs)) t, fs, Lambda vs newE)

  Apply e es -> do
    tv <- fresh
    (s1, t1, fs, newE) <- inferSynth envI envG e fills
    (ss, ts, fs', newEs, _) <- inferSynthListLTR envI (apply s1 envG) es fs
    s2 <- unify (apply ss t1) (Arrow ts tv)
    return (s2 `compose` ss `compose` s1, apply s2 tv, fs', Apply newE newEs)

  If e1 e2 e3 -> do
    (s1, t1, fs1, newC) <- inferSynth envI envG e1 fills
    (s2, t2, fs2, newT) <- inferSynth envI envG e2 fs1
    (s3, t3, fs3, newE) <- inferSynth envI envG e3 fs2
    s4 <- unify t1 (BaseType "Bool")
    s5 <- unify t2 t3
    return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, 
            apply s5 t2, 
            fs3,
            If newC newT newE)

  -- Let (Val x e1) e2 -> do
  --   (s1, t1, fs1) <- infer env e1 fills
  --   let env' = apply s1 env
  --       t' = generalize env' t1
  --   (s2, t2, fs2) <- infer (env' `extend` (x, t')) e2 fs1
  --   return (s1 `compose` s2, t2, fs2)

  -- Let (Rec x e1) e2 -> do
  --   nV <- fresh
  --   let expEnv = env `extend` (x, generalize env nV)
  --   (s1, t1, fs1) <- infer expEnv e1 fills
  --   let env' = apply s1 expEnv
  --       t' = generalize env' t1
  --   (s2, t2, fs2) <- infer (env' `extend` (x, t')) e2 fs1
  --   return (s1 `compose` s2, t2, fs2)

  Hole -> do 
    let (choice, newFills) = getFirstHole fills
    t <- if ("gen" `isPrefixOf` choice) then return (simpleLookup envG choice) else lookupAndInst envI choice
    return (nullSubst, t, newFills, Variable choice)

inferSynthDef :: TEnv -> Environment Type -> Defn -> Maybe [Name] -> Infer (Subst, Type, Maybe [Name], Expr)
inferSynthDef envI envG (Val x body) fills = do
    (s1, t1, newf, newB) <- inferSynth envI envG body fills
    return (s1, t1, newf, newB)

inferSynthListLTR :: TEnv -> Environment Type -> [Expr] -> Maybe [Name] -> Infer (Subst, Type, Maybe [Name], [Expr], Environment Type)
inferSynthListLTR envI envG exprs fills = foldM step (nullSubst, TTuple [], fills, [], envG) (reverse exprs)
  where step (ns, TTuple ts, fs, exprs, oldEnv) e = do
          (s, t, fs', nE) <- inferSynth envI oldEnv e fs
          return (s `compose` ns, TTuple (t:ts), fs', nE:exprs, apply s oldEnv)

-- inferSynthExpr :: TEnv -> EnvironmentExpr -> Maybe [Name] -> Maybe (Scheme, Maybe [Name], Expr)
-- inferExpr env expr fills = runInfer $ (infer envI envG expr fills)