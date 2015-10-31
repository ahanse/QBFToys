#!/bin/bash
#SBATCH -J qbf_b
#SBATCH -D /data/scratch/hansjoerg/QBF
#SBATCH -o qbfjob.%j.out
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8192M
#SBATCH --time=19:00:00
#SBATCH --mail-type=end
#SBATCH --mail-user=hansjoerg@zedat.fu-berlin.de

hostname
date 

/data/scratch/hansjoerg/QBF/run_bench.sh > ./benchmark.log
