#!/bin/bash -ex
#
# Create a docker virtual machine and run genlog job on it

# generate 16 digit random job id
JOB_ID=$(cat /dev/urandom | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)

S3_URL='s3://edechter.genlog'

# run 
docker run \
    --rm \
    --env-file env_file \
    -t edechter/genlog  \
    '/home/genlog/GenLog/ec2/job.sh' "$JOB_ID" "$@"

# fetch output data from s3
S3_OUT_URL="${S3_URL}/data/data_job_id_${JOB_ID}/job.out"
TMP_OUT_LOCAL="/tmp/genlog_data_${S3_OUT_URL}"
aws s3 cp "$S3_OUT_PATH" "$TMP_OUT_LOCAL"
cat "$TMP_OUT_LOCAL"

