module Dijkstra
    ( dijkstra
    , Distance
    , Distances
    , infinity
    ) where

import Graph
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromMaybe)

type Distance = Int
type Distances = Map Node Distance

-- Constant for infinity
infinity :: Distance
infinity = maxBound `div` 2

-- Sequential implementation of Dijkstra's algorithm
dijkstra :: Graph -> Node -> Distances
dijkstra graph source =
    let allNodes = nodes graph
        initialDistances = Map.fromList [(n, if n == source then 0 else infinity) | n <- allNodes]
        initialUnvisited = Set.fromList allNodes
    in dijkstraLoop graph initialDistances initialUnvisited

-- Main loop of Dijkstra's algorithm
dijkstraLoop :: Graph -> Distances -> Set Node -> Distances
dijkstraLoop graph distances unvisited
    | Set.null unvisited = distances
    | otherwise =
        let -- Find node with minimum distance among unvisited
            current = minimumUnvisited distances unvisited
            currentDist = Map.findWithDefault infinity current distances
        in if currentDist == infinity
           then distances  -- All remaining nodes are unreachable
           else
               let -- Get neighbors of current node
                   neighs = neighbors graph current
                   -- Update distances to neighbors
                   newDistances = foldr (relaxEdge currentDist) distances neighs
                   -- Mark current node as visited
                   newUnvisited = Set.delete current unvisited
               in dijkstraLoop graph newDistances newUnvisited

-- Find node with minimum distance among unvisited
minimumUnvisited :: Distances -> Set Node -> Node
minimumUnvisited distances unvisited =
    Set.foldr (\node minNode ->
        let nodeDist = Map.findWithDefault infinity node distances
            minDist = Map.findWithDefault infinity minNode distances
        in if nodeDist < minDist then node else minNode
    ) (Set.elemAt 0 unvisited) unvisited

-- Edge relaxation (distance update)
relaxEdge :: Distance -> (Node, Weight) -> Distances -> Distances
relaxEdge currentDist (neighbor, weight) distances =
    let oldDist = Map.findWithDefault infinity neighbor distances
        newDist = currentDist + weight
    in if newDist < oldDist
       then Map.insert neighbor newDist distances
       else distances
