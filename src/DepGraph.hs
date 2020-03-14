module DepGraph where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace

-- A dependency graph has the invariant that it is a DAG

type Node = String
type Edge = (Node, Node)

type DepGraph = Map.Map Node [Node]

emptyGraph :: DepGraph
emptyGraph = Map.empty

-- if the edge we are about to add creates a cycle
-- signal via Nothing
addEdge :: DepGraph -> Edge -> Maybe DepGraph
addEdge g (s, d) 
    | s == d || hasPath g d s = Nothing
    | otherwise =
        case Map.lookup s g of
            Just neigh -> Just $ Map.insert s (d:neigh) g
            Nothing -> Just $ Map.insert s [d] g

neighbours :: DepGraph -> Node -> [Node]
neighbours g n = 
    case Map.lookup n g of
        Just nei -> nei
        Nothing -> []

-- can be optimized further, does not shortcircuit
hasPath :: DepGraph -> Node -> Node -> Bool
hasPath g s d =
    if (d `elem` (neighbours g s) || s == d)
    then True
    else any (\a -> a == True) ([hasPath g crt d | crt <- neighbours g s])

topoSort :: DepGraph -> [Node]
topoSort depGraph = snd $ foldl traversal (Set.empty, []) (Map.keys depGraph)
    where
        traversal (visited, ordering) node = 
            if node `Set.member` visited
            then (visited, ordering)
            else addNode node $ foldl traversal (Set.insert node visited, ordering) (neighbours depGraph node)
        addNode node (v, o) = (v, node:o)