#!/bin/bash 
# set -x

echo "job.sh: Running..."
EXEC_PATH="${GENLOG_ROOT}/experiments/scripts/learn_number_morph/number_loglike.pl"

RESULT_PATH="$1"
S3_GL_URL="$2"
NUMBERS="${@:3:$#}"

GL_FILE=/tmp/ovbem.gl

if [[ -z $RESULT_PATH ]]; then 
    echo "job.sh: ** First argument empty. Result file path required. **" >&2
    echo "job.sh: stopped"
    exit 1
fi


if [[ -z "$S3_GL_URL" ]]; then 
    echo "job.sh: ** Second argument empty. AWS S3 URL to gzipped GenLog file required. **" >&2
    echo "job.sh: stopped"
    exit 1
else
    aws s3 cp "$S3_GL_URL" $GL_FILE.gz
    gunzip -v $GL_FILE.gz
    if [[ $? -ne 0 ]]; then 
        echo "job.sh: ** Unable to retrieve gl file $S3_GL_URL" >&2
        exit 1
    else 
        echo "job.sh: gl file $S3_GL_URL retrieved and uncompressed"
    fi
fi

if [ ${#NUMBERS[@]} -eq 0 ];  then 
    echo "job.sh: ** No numbers provided. **" >&2
    exit 1
fi


if [[ ! -x ${EXEC_PATH} ]] 
then
    echo "job.sh: ** Cannot run $EXEC_PATH **" >&2
else
    CMD="$EXEC_PATH $RESULT_PATH $GL_FILE $NUMBERS" # > ${GENLOG_ROOT}/ec2/job.out"
    echo "job.sh: executing $CMD"
    $CMD
fi

