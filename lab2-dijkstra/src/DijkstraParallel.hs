module DijkstraParallel
    ( dijkstraParallel
    ) where

import Graph
import Dijkstra (Distance, Distances, infinity)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Parallel.Strategies
import Data.Maybe (fromMaybe)

-- Parallel implementation of Dijkstra's algorithm
dijkstraParallel :: Graph -> Node -> Distances
dijkstraParallel graph source =
    let allNodes = nodes graph
        initialDistances = Map.fromList [(n, if n == source then 0 else infinity) | n <- allNodes]
        initialUnvisited = Set.fromList allNodes
    in dijkstraParallelLoop graph initialDistances initialUnvisited

-- Main loop of parallel Dijkstra's algorithm
dijkstraParallelLoop :: Graph -> Distances -> Set Node -> Distances
dijkstraParallelLoop graph distances unvisited
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
                   -- PARALLEL update of distances to neighbors
                   newDistances = relaxEdgesParallel currentDist neighs distances
                   -- Mark current node as visited
                   newUnvisited = Set.delete current unvisited
               in dijkstraParallelLoop graph newDistances newUnvisited

-- Find node with minimum distance among unvisited
minimumUnvisited :: Distances -> Set Node -> Node
minimumUnvisited distances unvisited =
    Set.foldr (\node minNode ->
        let nodeDist = Map.findWithDefault infinity node distances
            minDist = Map.findWithDefault infinity minNode distances
        in if nodeDist < minDist then node else minNode
    ) (Set.elemAt 0 unvisited) unvisited

-- Parallel relaxation of all edges
relaxEdgesParallel :: Distance -> [(Node, Weight)] -> Distances -> Distances
relaxEdgesParallel currentDist neighs distances =
    let -- Compute new distances in parallel
        updates = map (computeUpdate currentDist distances) neighs
                  `using` parList rseq
        -- Apply updates to distance map
    in foldr applyUpdate distances updates
  where
    -- Compute possible update for one neighbor
    computeUpdate :: Distance -> Distances -> (Node, Weight) -> Maybe (Node, Distance)
    computeUpdate curDist dists (neighbor, weight) =
        let oldDist = Map.findWithDefault infinity neighbor dists
            newDist = curDist + weight
        in if newDist < oldDist
           then Just (neighbor, newDist)
           else Nothing

    -- Apply update to map
    applyUpdate :: Maybe (Node, Distance) -> Distances -> Distances
    applyUpdate Nothing dists = dists
    applyUpdate (Just (node, dist)) dists = Map.insert node dist dists

-- Alternative version with more aggressive parallelization
-- for large graphs
dijkstraParallelAggressive :: Graph -> Node -> Distances
dijkstraParallelAggressive graph source =
    let allNodes = nodes graph
        initialDistances = Map.fromList [(n, if n == source then 0 else infinity) | n <- allNodes]
        initialUnvisited = Set.fromList allNodes
    in dijkstraParallelLoopAggressive graph initialDistances initialUnvisited

dijkstraParallelLoopAggressive :: Graph -> Distances -> Set Node -> Distances
dijkstraParallelLoopAggressive graph distances unvisited
    | Set.null unvisited = distances
    | otherwise =
        let current = minimumUnvisited distances unvisited
            currentDist = Map.findWithDefault infinity current distances
        in if currentDist == infinity
           then distances
           else
               let neighs = neighbors graph current
                   -- Use rpar for more aggressive parallelization
                   newDistances = relaxEdgesAggressiveParallel currentDist neighs distances
                   newUnvisited = Set.delete current unvisited
               in dijkstraParallelLoopAggressive graph newDistances newUnvisited

-- Aggressive parallel relaxation
relaxEdgesAggressiveParallel :: Distance -> [(Node, Weight)] -> Distances -> Distances
relaxEdgesAggressiveParallel currentDist neighs distances =
    let updates = map (computeUpdate currentDist distances) neighs
                  `using` parList rpar
    in foldr applyUpdate distances updates
  where
    computeUpdate curDist dists (neighbor, weight) =
        let oldDist = Map.findWithDefault infinity neighbor dists
            newDist = curDist + weight
        in if newDist < oldDist
           then Just (neighbor, newDist)
           else Nothing

    applyUpdate Nothing dists = dists
    applyUpdate (Just (node, dist)) dists = Map.insert node dist dists
