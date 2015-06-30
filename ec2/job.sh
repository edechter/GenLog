#!/bin/bash -ex
# set -ex
#
# Runs a genlog job in a genlog docker container. 
# ----------------------------------------------------------------------

# Constants
S3_URL="s3://edechter.genlog"
GL_PATH='/tmp/tmp.gl'

# Input arguments
JOB_ID=$1
GL_JOB_ID=$2
GL_ITER=$3
NUMBERS="${@:4:$#}"

echo "job.sh: Running..." 
RUNNER_PATH="${GENLOG_ROOT}/experiments/scripts/learn_number_morph/runner.pl"

# Make data dir for script data
DATA_DIR=/tmp/data_job_id_${JOB_ID}
mkdir -p ${DATA_DIR}
DATA_PATH="${DATA_DIR}/job.out"

# number arguments 
if [ ${#NUMBERS[@]} -eq 0 ];  then 
    echo "job.sh: ** No numbers provided. **" 
    exit 1
fi

# run executable
if [[ ! -x ${EXEC_PATH} ]] 
then
    echo "job.sh: ** Cannot run $EXEC_PATH **" 
else
    CMD="$EXEC_PATH $DATA_PATH $GL_PATH $NUMBERS"
    echo "job.sh: executing $CMD"
    $CMD
fi

cp -fvr "$DATA_DIR" "${HOME}/data/data_job_id_${JOB_ID}" 


