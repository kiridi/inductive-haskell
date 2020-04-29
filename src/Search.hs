{-# LANGUAGE TemplateHaskell #-}

module Search where

import Elements
import Data.List
import Data.Char
import Language.Syntax 
import Language.Environment
import Language.Types
import Language.Infer
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
            | d == 0 && not (check crtState) = Nothing
            | check crtState == True         = Just (fst crtState)
            | check crtState == False        = selectFirstResult (dbSearch (d - 1) check next) (next crtState)
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
        onlyNames = [ name | name <- fibk pinf, "BK_" `isPrefixOf` name || "gen" `isPrefixOf` name]
        crtCounter = uid pinf
        stream = [ (fill (updatedPinf cProd pinf) def cProd, cProd) | 
                    (def, nHoles) <- specDefs,
                    cProd <- sequence ([onlyNames ++ ["gen" ++ show (crtCounter + n)] | n <- [1.. nHoles]]) ]
        updatedPinf cp pinf = 
            let newOnes = news cp in
            pinf { uid = crtCounter + length newOnes,
                   fibk = (fibk pinf) ++ newOnes }
        filterJusts ls mb = case mb of
            (Nothing, _) -> ls
            (Just (d, pi), fills) -> ((pushDefn d newIProg (news fills)), pi):ls
        news ls = filter (\name -> "gen" `isPrefixOf` name && 
                                   (read (drop 3 name) :: Int) > crtCounter) ls

applyMr :: (Defn, Metarule) -> (Defn, Int)
applyMr ((Val name expr), mr) = 
    case expr of 
        Empty -> (Val name (body mr), nargs mr)
        _     -> error "When assigning mrs, the body should be empty"

fill :: ProgInfo -> Defn -> [Name] -> Maybe (Defn, ProgInfo)
fill pinf (Val name body) withs =
    case foldM addEdge (fDepG pinf) (zip [n | n <- withs, "gen" `isPrefixOf` n] (repeat name)) of
        Nothing -> Nothing
        Just newDepG -> return (Val name (fst $ fillHoles body withs), pinf { fDepG = newDepG })

fillHoles :: Expr -> [Name] -> (Expr, [Name])
fillHoles e names = case e of
    Number n -> (Number n, names)
    Character c -> (Character c, names)
    Variable v -> (Variable v, names)
    Lambda vs expr -> let (e, ns) = fillHoles expr names in
                      (Lambda vs e, ns)
    Apply e es -> let (e', ns) = fillHoles e names in
                  let (es', ns') = fillHolesLTR es ns in
                  (Apply e' es', ns')
    If e1 e2 e3 -> let (e1', ns1) = fillHoles e1 names in
                   let (e2', ns2) = fillHoles e2 ns1 in
                   let (e3', ns3) = fillHoles e3 ns2 in
                   (If e1' e2' e3', ns3)
    Hole -> (Variable (head names), tail names)
    
fillHolesLTR :: [Expr] -> [Name] -> ([Expr], [Name])
fillHolesLTR [] n = ([], n)
fillHolesLTR (expr:exprs) names = (newExpr:restsE, restsN)
    where 
        (newExpr, newNames) = fillHoles expr names
        (restsE, restsN) = fillHolesLTR exprs newNames
