#!/bin/bash
#$ -S /bin/bash
#$ -j y
#$ -l arch=linux-x64
#$ -o /mnt/chrastil/data2/users/liz/MLINDIV3/bids_data/BIDS_data/gridlog
#$ -cwd
#$ -q shared.q
#$ -pe openmp 4
# --skip_bids_validation
# The above are parameters for the grid. Replace stark.q with whatever it is for your lab. 

umask 0

#singularity_image=/tmp/mribin/poldracklab_fmriprep_latest-2019-07-25-274e2dcb44a6.img
singularity_image=/tmp/mribin/poldracklab_fmriprep_1_5_10.simg #check if this is the latest version


echo Starting fmriprep on $HOSTNAME with $NSLOTS cores at `date`
echo Subject: $1
echo Extra: $2 #If you have any additional parameters.
unset PYTHONPATH
time singularity run --cleanenv \
    -B /tmp/mribin/freesurfer6_linux/license.txt:/opt/freesurfer/license.txt \
    -B /run/shm:/run/shm \
    -B /mnt/chrastil:/mnt/chrastil \
    $singularity_image \
    /mnt/chrastil/data2/users/liz/MLINDIV3/bids_data/BIDS_data /mnt/chrastil/data2/users/liz/MLINDIV3/bids_data/BIDS_data/derivatives \
    participant -w /mnt/chrastil/data2/users/liz/MLINDIV3/bids_data/work \
    --skip_bids_validation --participant-label sub-$1 --omp-nthreads 4 $2 

echo Finished at `date`
