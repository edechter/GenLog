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
SUBJOD_ID=$2

echo "job.sh: Running..." >&2
RUNNER_PATH="${GENLOG_ROOT}/experiments/scripts/learn_number_morph/runner.pl"
EXEC_PATH="${GENLOG_ROOT}/src/run.pl"

# Make data dir for script data
DATA_DIR="$HOME"/data/"$JOB_ID"/"$SUBJOD_ID"
mkdir -p "${DATA_DIR}"
chown genlog "$DATA_DIR"


# run executable
if [[ ! -x "${EXEC_PATH}" ]] 
then
    echo "job.sh: ** Cannot run $EXEC_PATH **" 
else
    CMD="$EXEC_PATH $RUNNER_PATH $DATA_DIR"
    echo "job.sh: executing $CMD" >&2
    $CMD
fi


