# Lab 2: Parallel Dijkstra's Algorithm in Haskell

## Description
This project implements both sequential and parallel versions of Dijkstra's shortest path algorithm in Haskell. The parallel version uses the `parallel` package to demonstrate performance improvements through parallelization.

## Quick Start

### Build with Stack

```bash
cd Lab2/lab2-dijkstra
stack build
```

### Run Examples

```bash
# Show help
stack run -- --help

# Run on small example graph
stack run -- --example

# Run single test with 1000 nodes
stack run -- --nodes 1000

# Run full benchmark with 4 threads
stack run -- +RTS -N4 -RTS --benchmark

# Run with all available CPU cores
stack run -- +RTS -N -RTS --benchmark

# Run with 8 threads and runtime statistics
stack run -- +RTS -N8 -s -RTS --nodes 5000
```

## Project Structure

```
lab2-dijkstra/
├── stack.yaml              # Stack configuration
├── package.yaml            # Project dependencies and metadata
├── README.md               # This file
├── app/
│   └── Main.hs            # Executable entry point with CLI
└── src/
    ├── Graph.hs           # Graph data structure and generation
    ├── Dijkstra.hs        # Sequential Dijkstra implementation
    ├── DijkstraParallel.hs # Parallel Dijkstra implementation
    └── Benchmark.hs       # Benchmarking utilities
```

## Features

- **Sequential Dijkstra**: Classic single-threaded implementation
- **Parallel Dijkstra**: Multi-threaded version using `parallel` strategies
- **Random Graph Generation**: Generate test graphs of various sizes
- **Comprehensive Benchmarking**: Compare performance across different graph sizes
- **Detailed Analysis**: Calculate speedup and efficiency metrics

## Command Line Options

- `-n, --nodes NUMBER`: Number of nodes in graph (default: 1000)
- `-t, --threads NUMBER`: Number of threads (default: 4)
- `-b, --benchmark`: Run full benchmark suite
- `-e, --example`: Run on small example graph
- `-h, --help`: Show help message

## Understanding RTS Options

GHC's Runtime System (RTS) options control parallel execution:

- `+RTS -N4 -RTS`: Use 4 cores
- `+RTS -N -RTS`: Use all available cores
- `+RTS -s -RTS`: Show runtime statistics
- `+RTS -N4 -s -RTS`: Use 4 cores and show statistics

## Dependencies

All dependencies are managed by Stack:

- `base >= 4.14`: Haskell standard library
- `containers >= 0.6`: Efficient data structures (Map, Set)
- `parallel >= 3.2`: Parallel programming strategies
- `deepseq >= 1.4`: Deep evaluation for benchmarking
- `clock >= 0.8`: High-precision timing
- `random >= 1.2`: Random number generation

Stack will automatically install GHC and all dependencies when you run `stack build`.

## Example Output

```
╔════════════════════════════════════════════════════════════╗
║       Lab 2: Dijkstra's Algorithm Parallelization          ║
╚════════════════════════════════════════════════════════════╝

===== EXAMPLE: SMALL GRAPH =====

Graph:
  Nodes: 0, 1, 2, 3, 4
  Edges:
    0 -> 1 (weight: 4)
    0 -> 2 (weight: 1)
    ...

Running sequential algorithm...
Result (distances from node 0):
  Node 0: 0
  Node 1: 3
  Node 2: 1
  ...

Verification: are results identical?
✓ YES - algorithms work correctly!

Done!
```

## Performance Notes

- Small graphs may show little or no speedup due to parallelization overhead
- Larger graphs (1000+ nodes) typically show better parallel performance
- Speedup depends on CPU core count and graph structure
- Dense graphs benefit more from parallelization

## Troubleshooting

If `stack build` fails, try:
```bash
stack clean
stack build
```

If you encounter memory issues with large graphs:
```bash
stack run -- +RTS -N4 -M2G -RTS --nodes 10000
```
(This limits memory to 2GB)
