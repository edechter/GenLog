#!/bin/bash -ex

RESULT_FILE=out.json
S3_DATA_PATH='s3://edechter.genlog/data/data_job_id_1435093342217/ovbem_gl_0001.gl'
echo $S3_DATA_PATH

docker run \
    --env-file env_file \
    -e JOB_ARGS="$RESULT_FILE $S3_DATA_PATH 1 2" -it edechter/genlog bash 
