#!/bin/bash -e
#
# Create a docker virtual machine and run genlog job on it

# generate 16 digit random job id
SUBJOB_ID=$(cat /dev/urandom | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)

S3_URL='s3://edechter.genlog'

# make data and log dirs
JOB_ID=$1
DATADIR="./data/${JOB_ID}"
LOGDIR="./logs/${JOB_ID}"
mkdir -p ./data/${JOB_ID}
mkdir -p ./logs/${JOB_ID}

# run 
docker run \
    --env-file env_file \
    -v $(pwd)/"$DATADIR"/:/home/genlog/data \
    -v $(pwd)/"$LOGDIR"/:/home/genlog/logs \
    edechter/genlog  \
    '/home/genlog/GenLog/ec2/job.sh' \
        "$JOB_ID" \
        "$SUBJOB_ID" \
        "${@:2:$#}" \
        # 1>>job.stdout 2>>job.stderr 


