#!/bin/bash -ex
#
# Create a docker virtual machine and run genlog job on it

# generate 16 digit random job id
JOB_ID=$(cat /dev/urandom | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)

S3_URL='s3://edechter.genlog'
GL_JOB_ID=1435093342217

# # create a docker machine
# docker-machine create \
#     --driver virtualbox \
#     genlog-dev

# load machine enviroment
eval "$(docker-machine env genlog-dev)"


# run 
docker run \
    --env-file env_file \
    -i \
    -t edechter/genlog  \
    # bash
    '/home/genlog/GenLog/ec2/job.sh' "$JOB_ID" "$GL_JOB_ID" 1 1 2
