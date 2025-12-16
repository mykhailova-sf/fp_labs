module Benchmark
    ( BenchResult(..)
    , timeit
    , runBenchmark
    , runFullBenchmark
    , printBenchResults
    , printComparisonTable
    , analyzeResults
    ) where

import Graph
import Dijkstra
import DijkstraParallel
import System.Clock
import Text.Printf
import Control.DeepSeq
import Data.List (sortBy)
import Data.Ord (comparing)

-- Benchmark result
data BenchResult = BenchResult
    { brName :: String
    , brGraphSize :: Int
    , brTime :: Double  -- in seconds
    , brThreads :: Int
    } deriving (Show, Eq)

-- Measure execution time of function
timeit :: NFData a => IO a -> IO Double
timeit action = do
    start <- getTime Monotonic
    result <- action
    result `deepseq` return ()
    end <- getTime Monotonic
    let diff = diffTimeSpec start end
    return $ fromIntegral (sec diff) + fromIntegral (nsec diff) / 1e9
  where
    diffTimeSpec t1 t2 = TimeSpec
        { sec = sec t2 - sec t1
        , nsec = nsec t2 - nsec t1
        }

-- Run single benchmark
runBenchmark :: String -> Graph -> Node -> IO BenchResult
runBenchmark name graph source = do
    let graphSize = length (nodes graph)
    putStrLn $ "Running: " ++ name ++ " (graph size: " ++ show graphSize ++ " nodes)"

    time <- if name == "Sequential"
            then timeit $ return $! dijkstra graph source
            else timeit $ return $! dijkstraParallel graph source

    putStrLn $ "  Execution time: " ++ printf "%.4f" time ++ " sec"

    return BenchResult
        { brName = name
        , brGraphSize = graphSize
        , brTime = time
        , brThreads = if name == "Sequential" then 1 else 0  -- 0 means "parallel"
        }

-- Full benchmark with different graph sizes
runFullBenchmark :: IO [(BenchResult, BenchResult)]
runFullBenchmark = do
    putStrLn "\n===== STARTING FULL BENCHMARK =====\n"

    let sizes = [100, 500, 1000, 2000]

    results <- mapM runSizeBenchmark sizes

    putStrLn "\n===== BENCHMARK COMPLETE =====\n"
    return results
  where
    runSizeBenchmark :: Int -> IO (BenchResult, BenchResult)
    runSizeBenchmark size = do
        putStrLn $ "\n--- Generating graph with " ++ show size ++ " nodes ---"
        graph <- generateRandomGraph size (size * 3) 1 100

        -- Sequential run
        seqResult <- runBenchmark "Sequential" graph 0

        -- Short pause between tests
        putStrLn "  (pause between tests...)"

        -- Parallel run
        parResult <- runBenchmark "Parallel" graph 0

        return (seqResult, parResult)

-- Print results of single benchmark
printBenchResults :: BenchResult -> IO ()
printBenchResults result = do
    printf "%s:\n" (brName result)
    printf "  Graph size: %d nodes\n" (brGraphSize result)
    printf "  Execution time: %.4f sec\n" (brTime result)

-- Print comparison table
printComparisonTable :: [(BenchResult, BenchResult)] -> IO ()
printComparisonTable results = do
    putStrLn "\n================================================================"
    putStrLn "            COMPARISON TABLE RESULTS"
    putStrLn "================================================================"
    putStrLn "  Size   |  Sequential |  Parallel   |  Speedup    |  Efficiency"
    putStrLn "  graph  |    (sec)    |    (sec)    |     (x)     |      (%)"
    putStrLn "----------------------------------------------------------------"

    mapM_ printRow results

    putStrLn "================================================================"
    putStrLn ""
    putStrLn "Speedup = Sequential_Time / Parallel_Time"
    putStrLn "Efficiency = Speedup / Thread_Count * 100%"
  where
    printRow :: (BenchResult, BenchResult) -> IO ()
    printRow (seqRes, parRes) =
        let size = brGraphSize seqRes
            seqTime = brTime seqRes
            parTime = brTime parRes
            speedup = seqTime / parTime
            -- Assume 4 threads for efficiency calculation
            efficiency = (speedup / 4.0) * 100
        in printf "  %7d | %11.4f | %11.4f | %11.2f | %11.2f%%\n"
                  size seqTime parTime speedup efficiency

-- Additional function for detailed analysis
analyzeResults :: [(BenchResult, BenchResult)] -> IO ()
analyzeResults results = do
    putStrLn "\n===== DETAILED ANALYSIS =====\n"

    let speedups = map (\(seq, par) -> brTime seq / brTime par) results
        avgSpeedup = sum speedups / fromIntegral (length speedups)
        maxSpeedup = maximum speedups
        minSpeedup = minimum speedups

    printf "Average speedup: %.2fx\n" avgSpeedup
    printf "Maximum speedup: %.2fx\n" maxSpeedup
    printf "Minimum speedup: %.2fx\n" minSpeedup

    putStrLn "\nConclusion:"
    if avgSpeedup > 1.5
        then putStrLn "[OK] Parallelization provides significant performance improvement!"
        else putStrLn "[WARNING] Parallelization has minor improvement (possibly due to overhead)"
