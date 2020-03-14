{-# LANGUAGE TemplateHaskell #-}

module Search where

import Elements
import Data.List
import Data.Char
import Language.Syntax 
import Language.Environment
import Language.Types
import Language.Infer
import Language.InferSynth
import Data.Maybe
import Debug.Trace
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import DepGraph
import Control.Monad

type ProgState = (IProgram, ProgInfo)

progSearch :: (ProgState -> Bool) -> (ProgState -> [ProgState]) -> ProgState -> Maybe IProgram
progSearch check next initInfo = 
    selectFirstResult (\d -> (trace ("Searching at depth " ++ show d ++ "...")) dbSearch d check next initInfo) [1 .. ]
    where dbSearch d check next crtState
            | d == 0                  = Nothing
            | check crtState == True  = Just (fst crtState)
            | check crtState == False = selectFirstResult (dbSearch (d - 1) check next) (next crtState)
          selectFirstResult select [] = Nothing
          selectFirstResult select (xm:xms) =
            case select xm of
                Nothing -> selectFirstResult select xms
                Just x  -> Just x

expand :: ProgState -> [ProgState]
expand (ip, pinf)  
    | not (isCompleteIP ip) = foldl filterJusts [] stream
    | otherwise = []
    where
        (defn, newIProg) = popCand ip
        specDefs = map applyMr (zip (repeat defn) (mrs pinf))
        onlyNames = [ name | (name, _) <- envToList (envI pinf), "BK_" `isPrefixOf` name ] ++
                    [ name | (name, _) <- envToList (envG pinf) ] 
        crtCounter = uid pinf
        stream = [ (fill (updatedPinf cProd pinf) def cProd, cProd) | 
                    (def, nHoles) <- specDefs,
                    cProd <- sequence ([ ("gen" ++ show (crtCounter + n)) : onlyNames | n <- [0.. nHoles - 1]]) 
                 ]
        updatedPinf cp pinf = 
            let generated = filter (\name -> "gen" `isPrefixOf` name) cp
                tags = [uid pinf .. uid pinf + (length generated) - 1] in
            if length generated > 0 then
            pinf { uid = (uid pinf) + length generated,
                   envG = defargs (envG pinf) generated (map (\t -> TVar ("unknown" ++ show t)) tags) }
            else pinf
        filterJusts ls mb = case mb of
            (Nothing, _) -> ls
            (Just (d, pi), fills) -> ((pushDefn d newIProg fills), pi):ls

applyMr :: (Defn, Metarule) -> (Defn, Int)
applyMr ((Val name expr), mr) = 
    case expr of 
        Empty -> (Val name (body mr), nargs mr)
        _     -> error "When assigning mrs, the body should be empty"

fill :: ProgInfo -> Defn -> [Name] -> Maybe (Defn, ProgInfo)
fill pinf (Val name body) withs = do
    newDepG <- foldM addEdge (fDepG pinf) (zip (repeat name) withs)
    (sub1, typ, populated) <- runStepInfer (uid pinf) $ inferSynthDef (envI pinf) (envG pinf) (Val name body) (Just withs)
    sub2 <- canUnify (simpleLookup (envG pinf) name) typ
    let newPinf = pinf {
        envG = apply (sub2 `compose` sub1) (define (envG pinf) name typ),
        fDepG = newDepG
    }
    _ <- {-trace (show populated ++ "\n" ++ show typ ++ "\n" ++ show (envG pinf) ++ "\n" ++ show (envG newPinf) ++ "\n" ++ show sub1 ++ "\n" ++ show sub2)-} canUnify (expType pinf) (simpleLookup (envG newPinf) "gen0")
    return (Val name populated, newPinf)
