#!/bin/env bash
#SBATCH -J Sim_study
#SBATCH -N 1
#SBATCH --mem=6GB
#SBATCH --time=55:00:00
#SBATCH --output=sim_study_%J.out
#SBATCH --error=sim_study_%J.err

module load statistical/R/4.0.2/gcc.8.3.1

Rscript --vanilla MIXED_MainSimulationSLURM_new.R $i $j