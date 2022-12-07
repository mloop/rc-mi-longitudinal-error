#!/bin/bash

#SBATCH --mem-per-cpu=100M
#SBATCH --job-name=me-sim
#SBATCH -p general
#SBATCH --array=1-1000
#SBATCH --cpus-per-task=1
#SBATCH -t 1:00:00
#SBATCH --mail-type=END,FAIL

module load R/4.2.1
module load gcc/8.4.0

Rscript 05_02_create_imputations.R $SLURM_ARRAY_TASK_ID

