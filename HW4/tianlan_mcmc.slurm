#!/bin/bash
#SBATCH -J Pset4_MCMC # name for job array
#SBATCH -o all.out #Standard output
#SBATCH -e all.err #Standard error
#SBATCH -p stats #Partition
#SBATCH -t 01:00:00 #Running time of 1 hour
#SBATCH --mem-per-cpu 3000 #Memory request
#SBATCH -n 1 #Number of cores
#SBATCH -N 1 #All cores on one machine
#SBATCH --mail-type=END      #Type of email notification- BEGIN,END,FAIL,ALL
#SBATCH --mail-user=tianlan@g.harvard.edu  #Email to which notifications will be sent

# first arg = job.id

Rscript tianlan_mcmc.R $SLURM_ARRAY_TASK_ID
