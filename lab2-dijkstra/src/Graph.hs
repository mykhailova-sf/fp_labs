module Graph
    ( Node
    , Weight
    , Graph
    , Edge
    , createGraph
    , addEdge
    , neighbors
    , nodes
    , generateRandomGraph
    , exampleGraph1
    , exampleGraph2
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Random

-- Data types
type Node = Int
type Weight = Int
type Edge = (Node, Node, Weight)
type Graph = Map Node [(Node, Weight)]

-- Create empty graph
createGraph :: Graph
createGraph = Map.empty

-- Add edge to graph
addEdge :: Node -> Node -> Weight -> Graph -> Graph
addEdge from to weight graph =
    Map.insertWith (++) from [(to, weight)] graph

-- Get neighbors of node
neighbors :: Graph -> Node -> [(Node, Weight)]
neighbors graph node = Map.findWithDefault [] node graph

-- Get all nodes of graph
nodes :: Graph -> [Node]
nodes = Map.keys

-- Generate random graph
generateRandomGraph :: Int -> Int -> Int -> Int -> IO Graph
generateRandomGraph numNodes numEdges minWeight maxWeight = do
    gen <- newStdGen
    let allNodes = [0..numNodes-1]
        randomEdges = take numEdges $ generateEdges gen allNodes minWeight maxWeight
    return $ buildGraph randomEdges
  where
    generateEdges :: StdGen -> [Node] -> Int -> Int -> [Edge]
    generateEdges gen nodeList minW maxW =
        let (g1, g2) = split gen
            (g3, g4) = split g2
            fromNodes = randomRs (0, length nodeList - 1) g1
            toNodes = randomRs (0, length nodeList - 1) g3
            weights = randomRs (minW, maxW) g4
            edges = zipWith3 (\f t w -> (nodeList !! f, nodeList !! t, w)) fromNodes toNodes weights
        in filter (\(f, t, _) -> f /= t) edges  -- Avoid self-loops

    buildGraph :: [Edge] -> Graph
    buildGraph edges = foldr (\(f, t, w) g -> addEdge f t w g) createGraph edges

-- Example graph 1 (small graph for testing)
exampleGraph1 :: Graph
exampleGraph1 =
    let edges = [ (0, 1, 4)
                , (0, 2, 1)
                , (2, 1, 2)
                , (1, 3, 1)
                , (2, 3, 5)
                , (3, 4, 3)
                ]
    in foldr (\(f, t, w) g -> addEdge f t w g) createGraph edges

-- Example graph 2 (slightly larger)
exampleGraph2 :: Graph
exampleGraph2 =
    let edges = [ (0, 1, 7)
                , (0, 2, 9)
                , (0, 5, 14)
                , (1, 2, 10)
                , (1, 3, 15)
                , (2, 3, 11)
                , (2, 5, 2)
                , (3, 4, 6)
                , (4, 5, 9)
                ]
    in foldr (\(f, t, w) g -> addEdge f t w g) createGraph edges
