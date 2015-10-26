#!/bin/bash

./QBFToys/runBenchmarks.py start -p "Benchmarks/preprocessors.csv" -r "Benchmarks/results.csv" -s "Benchmarks/solvers.csv" -v 2500 -c 2500 -t 120 -q "./depqbf --max-secs=%t %f" -a 0 Benchmarks/BenchmarkFiles/output
