#!/bin/bash

#SBATCH --mem-per-cpu=1GB
#SBATCH --job-name=full_stochastic_mi
#SBATCH -p general
#SBATCH --array=1-1000
#SBATCH -t 00:10:00
#SBATCH --mail-type=END,FAIL

module load gcc/9.3.0
module load R/4.2.3

Rscript 05_02_01_create_full_stochastic_imputations.R $SLURM_ARRAY_TASK_ID

