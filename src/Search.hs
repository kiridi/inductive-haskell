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
                    cProd <- sequence ([onlyNames ++ (("gen" ++ show (crtCounter + n)):names (envG pinf)) | n <- [1.. nHoles]]) 
                 ]
        updatedPinf cp pinf = 
            let generated = filter (\name -> "gen" `isPrefixOf` name) cp
                tags = [uid pinf .. uid pinf + (length generated) - 1] in
            if length generated > 0 then
            pinf { uid = (uid pinf) + length generated,
                   envG = defargs (envG pinf) generated (map (\t -> TVar ("unk" ++ show t)) tags) }
            else pinf
        filterJusts ls mb = case mb of
            (Nothing, _) -> ls
            (Just (d, pi), fills) -> ((pushDefn d newIProg fills), pi):ls

applyMr :: (Defn, Metarule) -> (Defn, Int)
applyMr ((Val name expr), mr) = 
    case expr of 
        Empty -> (Val name (body mr), nargs mr)
        _     -> error "When assigning mrs, the body should be empty"

-- the problem is with the first can Unify: because variable overlap, can't univy or smth
fill :: ProgInfo -> Defn -> [Name] -> Maybe (Defn, ProgInfo)
fill pinf (Val name body) withs = {-trace (if (withs == ["gen2"] || name == "gen2") then (show withs ++ show (apply sub1 $ envG pinf) ++ show (sub1, typ)) else "") $-} do
    newDepG <- foldM addEdge (fDepG pinf) (zip (repeat name) withs)
    (sub1, typ, populated, newUID) <- runStepInfer (uid pinf) $ inferSynthDef (envI pinf) (envG pinf) (Val name body) (Just withs)
    sub2 <- trace (if (withs == ["gen3"]) then (show withs ++ show (apply sub1 $ envG pinf) ++ show (sub1, typ)) else "") $ canUnify (simpleLookup (apply sub1 (envG pinf)) name) typ
    let newPinf = pinf {
        envG = apply (sub2 `compose` sub1) (define (envG pinf) name typ),
        fDepG = newDepG,
        uid = newUID
    }
    _ <- canUnify (expType pinf) (simpleLookup (envG newPinf) "gen0")
    return (Val name populated, newPinf)
    
