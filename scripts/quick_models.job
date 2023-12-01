#!/bin/bash

#SBATCH --job-name=quick-models
#SBATCH -p general
#SBATCH --array=1-5000
#SBATCH --mem=1g
#SBATCH -t 00:10:00
#SBATCH --mail-type=END,FAIL
##SBATCH --output=../output/quick_models_out/%A_%a.out

set -o errexit

module load gcc/9.3.0
module load R/4.2.3

Rscript 02_naive.R $SLURM_ARRAY_TASK_ID
Rscript 03_complete_case.R $SLURM_ARRAY_TASK_ID
Rscript 07_true_model.R $SLURM_ARRAY_TASK_ID
