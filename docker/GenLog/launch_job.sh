#!/bin/bash -ex
#
# Create a docker virtual machine and run genlog job on it

# generate 16 digit random job id
SUBJOB_ID=$(cat /dev/urandom | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)
JOB_ID=$1
S3_URL='s3://edechter.genlog'


swarm_master_dir="$(docker-machine ssh "$DOCKER_MACHINE_NAME" pwd)"
data_dir="$swarm_master_dir"/data
logs_dir="$swarm_master_dir"/logs

docker-compose \
    --verbose \
    -f "${GENLOG_ROOT}"/docker/compose_genlog/docker-compose.yml \
    up \
    -d 


# # run 
# docker run \
#     -d \
#     --env-file env_file \
#     --volumes-from data \
#     --volumes-from logs \
#     edechter/genlog  \
#     '/home/genlog/GenLog/ec2/job.sh' \
#         "$JOB_ID" \
#         "$SUBJOB_ID" \
#         "${@:2:$#}" \
#         1>>job.stdout 2>>job.stderr 


