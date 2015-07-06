#!/bin/bash -ex
# set -ex
#
# Runs a genlog job in a genlog docker container. 
# ----------------------------------------------------------------------

IN_FILE="$1"
OUT_FILE="$2"


EXEC_PATH="${GENLOG_ROOT}/experiments/scripts/learn_number_morph/number_loglike.pl"
if [[ ! -f "${EXEC_PATH}" ]]; then 
    echo "** Error: cannot find $EXEC_PATH **"
    exit 1
fi

# Constants
S3_URL="s3://edechter.genlog"
GL_PATH='/tmp/tmp.gl'

# Input arguments
JOB_ID="$1"
SUBJOD_ID="$2"
S3_GL_URL="$3"
NUMBERS="${@:4:$#}"

echo "job.sh: Running..." >&2

# Make data dir for script data
sudo chown genlog "$HOME"/data
sudo chown genlog "$HOME"/logs
DATA_DIR="$HOME"/data/"$JOB_ID"/"$SUBJOD_ID"
mkdir -p "${DATA_DIR}"
DATA_FILE="${DATA_DIR}"/out.json

# get gl file from s3
aws s3 cp "$S3_GL_URL" ./in.gl.gz
if [[ $? -ne 0 ]] 
then
    echo "job.sh: ** Cannot fetch $S3_GL_URL. **"
    exit 1 
fi

# gunzip file
gunzip ./in.gl.gz
if [[ ?! -ne 0 ]] 
then
    echo "job.sh: ** Could not uncompress ./in.gl.gz **" 
    exit 1
fi





# run executable
if [[ ! -x "${EXEC_PATH}" ]] 
then
    echo "job.sh: ** Cannot run $EXEC_PATH **" 
else
    CMD="${EXEC_PATH} ${DATA_FILE} $(pwd)/in.gl $NUMBERS"
    echo "job.sh: executing $CMD" >&2
    $CMD
fi


