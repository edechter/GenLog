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
EXEC_PATH="${GENLOG_ROOT}/experiments/scripts/learn_number_morph/number_loglike.pl"

# build gl url
S3_GL_URL="${S3_URL}"
S3_GL_URL+="/data/data_job_id_${GL_JOB_ID}"
S3_GL_URL+="/ovbem_gl_$(printf "%04d\n" ${GL_ITER}).gl.gz"

# fetch gl.gz file 
aws s3 cp "$S3_GL_URL" $GL_PATH.gz
gunzip -fv $GL_PATH.gz
if [[ $? -ne 0 ]]; then 
    echo "job.sh: * Unable to retrieve gl file $S3_GL_URL" 
    exit 1
else 
    echo "job.sh: gl file $S3_GL_URL retrieved and uncompressed" 
fi

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


