{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Graph
import Dijkstra
import DijkstraParallel
import Benchmark
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import Text.Printf
import qualified Data.Map.Strict as Map

-- Command line options
data Options = Options
    { optNodes :: Int
    , optThreads :: Int
    , optBenchmark :: Bool
    , optHelp :: Bool
    , optExample :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optNodes = 1000
    , optThreads = 4
    , optBenchmark = False
    , optHelp = False
    , optExample = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['n'] ["nodes"]
        (ReqArg (\n opts -> opts { optNodes = read n }) "NUMBER")
        "Number of nodes in graph (default: 1000)"
    , Option ['t'] ["threads"]
        (ReqArg (\t opts -> opts { optThreads = read t }) "NUMBER")
        "Number of threads (default: 4)"
    , Option ['b'] ["benchmark"]
        (NoArg (\opts -> opts { optBenchmark = True }))
        "Run full benchmark"
    , Option ['e'] ["example"]
        (NoArg (\opts -> opts { optExample = True }))
        "Run on example graph"
    , Option ['h'] ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "Show this help"
    ]

-- Parse command line options
parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: lab2-dijkstra [OPTIONS...]"

-- Print help
printHelp :: IO ()
printHelp = do
    putStrLn $ usageInfo header options
    putStrLn "\nUsage examples:"
    putStrLn "  lab2-dijkstra --example                    # Run on small example"
    putStrLn "  lab2-dijkstra --nodes 1000 --threads 4     # Run with 1000 nodes on 4 threads"
    putStrLn "  lab2-dijkstra --benchmark                  # Run full benchmark"
    putStrLn "\nCompilation:"
    putStrLn "  stack build"
    putStrLn "\nRun with RTS options:"
    putStrLn "  lab2-dijkstra +RTS -N4 -RTS --benchmark    # 4 threads"
    putStrLn "  lab2-dijkstra +RTS -N8 -s -RTS --nodes 5000 # 8 threads with statistics"
  where header = "Lab 2: Dijkstra's Algorithm Parallelization\n"

-- Run on example graph
runExample :: IO ()
runExample = do
    putStrLn "\n===== EXAMPLE: SMALL GRAPH =====\n"
    let graph = exampleGraph1
        source = 0

    putStrLn "Graph:"
    putStrLn "  Nodes: 0, 1, 2, 3, 4"
    putStrLn "  Edges:"
    putStrLn "    0 -> 1 (weight: 4)"
    putStrLn "    0 -> 2 (weight: 1)"
    putStrLn "    2 -> 1 (weight: 2)"
    putStrLn "    1 -> 3 (weight: 1)"
    putStrLn "    2 -> 3 (weight: 5)"
    putStrLn "    3 -> 4 (weight: 3)"

    putStrLn "\nRunning sequential algorithm..."
    let seqResult = dijkstra graph source
    putStrLn "Result (distances from node 0):"
    printDistances seqResult

    putStrLn "\nRunning parallel algorithm..."
    let parResult = dijkstraParallel graph source
    putStrLn "Result (distances from node 0):"
    printDistances parResult

    putStrLn "\nVerification: are results identical?"
    if seqResult == parResult
        then putStrLn "[YES] - algorithms work correctly!"
        else putStrLn "[NO] - there are discrepancies!"

-- Print distances
printDistances :: Distances -> IO ()
printDistances distances = do
    let distList = Map.toList distances
    mapM_ (\(node, dist) ->
        if dist == infinity
            then printf "  Node %d: infinity (unreachable)\n" node
            else printf "  Node %d: %d\n" node dist
        ) distList

-- Run single test
runSingleTest :: Int -> Int -> IO ()
runSingleTest numNodes numThreads = do
    printf "\n===== TEST: %d NODES, %d THREADS =====\n\n" numNodes numThreads

    putStrLn "Generating random graph..."
    graph <- generateRandomGraph numNodes (numNodes * 3) 1 100
    let source = 0

    putStrLn "\nRunning sequential algorithm..."
    seqTime <- timeit $ return $! dijkstra graph source
    printf "Execution time: %.4f sec\n" seqTime

    putStrLn "\nRunning parallel algorithm..."
    parTime <- timeit $ return $! dijkstraParallel graph source
    printf "Execution time: %.4f sec\n" parTime

    let speedup = seqTime / parTime
        efficiency = (speedup / fromIntegral numThreads) * 100

    printf "\n--- RESULTS ---\n"
    printf "Speedup: %.2fx\n" speedup
    printf "Efficiency: %.2f%%\n" efficiency

    if speedup > 1.0
        then putStrLn "[OK] Parallel version is faster!"
        else putStrLn "[WARNING] Sequential version is faster (graph may be too small)"

-- Main function
main :: IO ()
main = do
    args <- getArgs
    (opts, _) <- parseOptions args

    putStrLn "=============================================================="
    putStrLn "       Lab 2: Dijkstra's Algorithm Parallelization"
    putStrLn "=============================================================="

    if optHelp opts
        then printHelp
        else if optExample opts
            then runExample
            else if optBenchmark opts
                then do
                    results <- runFullBenchmark
                    printComparisonTable results
                    analyzeResults results
                else
                    runSingleTest (optNodes opts) (optThreads opts)

    putStrLn "\nDone!"
