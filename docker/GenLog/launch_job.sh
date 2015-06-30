#!/bin/bash -ex
#
# Create a docker virtual machine and run genlog job on it

# generate 16 digit random job id
JOB_ID=$(cat /dev/urandom | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)

S3_URL='s3://edechter.genlog'

# run 
docker run \
    --env-file env_file \
    -v $(pwd)/data:/home/genlog/data \
    -v $(pwd)/logs:/home/genlog/logs \
    edechter/genlog  \
    '/home/genlog/GenLog/ec2/job.sh' "$JOB_ID" "$@"


