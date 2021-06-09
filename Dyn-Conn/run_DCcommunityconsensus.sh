#!/bin/bash
# Setup qsub options
#  Nothing needed for this
#SBATCH --job-name=rundccc_bs3
#SBATCH -c 4
#SBATCH -o /mnt/chrastil/lab/users/rob/projects/DynConn/slurmlog/rundccc-%j.out
#SBATCH --partition=standard

echo This was run at: `date`

echo Job ID: $SLURM_JOB_ID
echo Job name: $SLURM_JOB_NAME
echo Submit host: $SLURM_SUBMIT_HOST
echo "Node(s) used": $SLURM_JOB_NODELIST

umask 0
unset PYTHONPATH

analysis_dir=/mnt/chrastil/lab/users/rob/projects/DynConn
sub=$1          # Subject ID
gam=$2          # Spatial Resolution parameter
ome=$3          # Temporal Stickiness Parameter
n_partitions=$4 # Number of Partitions to iterate through, and select extract consensus partition from
​
​echo $analysis_dir
echo ID $sub
echo Starting rundccc on $HOSTNAME with $SLURM_CPUS_PER_TASK at `date`
​
/tmp/mribin/matlab/R2019b/bin/matlab -nojvm -r "cd('$analysis_dir');run_DCcommunityconsensus($sub,$gam,$ome,$n_partitions);exit";

