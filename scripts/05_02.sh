#!/bin/bash

#SBATCH --mem-per-cpu=1GB
#SBATCH --job-name=pmm
#SBATCH -p general
#SBATCH --array=1-1000
#SBATCH -t 00:05:00
#SBATCH --mail-type=END,FAIL

module load gcc/9.3.0
module load R/4.2.3

Rscript 05_02_create_imputations.R $SLURM_ARRAY_TASK_ID

