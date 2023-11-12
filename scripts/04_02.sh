#!/bin/bash

#SBATCH --mem-per-cpu=500m
#SBATCH --job-name=snipe-bootstrap
#SBATCH -p general
#SBATCH --array=1-1000
#SBATCH -t 00:20:00
#SBATCH --mail-type=END,FAIL

module load gcc/9.3.0
module load R/4.2.3

Rscript 04_02_snipe_bootstrap.R $SLURM_ARRAY_TASK_ID
