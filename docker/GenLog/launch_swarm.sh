#!/bin/bash -ex
#
# Launch a swarm of docker machines
# ----------------------------------------------------------------------

NUMBER_AGENTS=1
# DRIVER=virtualbox
DRIVER=amazonec2

# ----------------------------------------------------------------------

source env_file
SWARM_TOKEN="$(docker run swarm create)"
export SWARM_TOKEN


if [[ "$DRIVER" = amazonec2 ]]; then 
    driver_label=ec2
elif [[ "$DRIVER" = virtualbox ]]; then 
    driver_label=local
else
    driver_label="$DRIVER"
fi

swarm_master_name="swarm-master-${driver_label}"



docker-machine create \
    -d "$DRIVER" \
    --amazonec2-access-key="$AWS_ACCESS_KEY" \
    --amazonec2-secret-key="$AWS_SECRET_KEY" \
    --amazonec2-instance-type=t2.small \
    --amazonec2-vpc-id="$AWS_VPC_ID" \
    --swarm \
    --swarm-master \
    --swarm-discovery token://"$SWARM_TOKEN" \
    swarm-master-"${driver_label}" & 

# create swarm agents
parallel  \
    docker-machine create \
    -d "$DRIVER" \
    --amazonec2-access-key="$AWS_ACCESS_KEY" \
    --amazonec2-secret-key="$AWS_SECRET_KEY" \
    --amazonec2-instance-type=t2.small \
    --amazonec2-vpc-id="$AWS_VPC_ID" \
    --swarm \
    --swarm-discovery token://"$SWARM_TOKEN" \
    swarm-agent-"${driver_label}"-{} \
    ::: "$(seq -f "%03.0f" 1 "$NUMBER_AGENTS")"

# connect to swarm-master
eval "$(docker-machine env --swarm "${swarm_master_name}")"
if [[ $? -ne 0 ]]; then 
    echo "** Error: unable to connect to ${swarm_master_name}" > &2
    exit 1
fi

# create data and logs directory on master
docker-machine ssh "${swarm_master_name}" \
    'mkdir -p ~/data \
     && mkdir -p ~/logs'

if [[ $? -ne 0 ]]; then 
    echo "** Error: unable to create ~/data and ~/logs directory" > &2
    exit 1
fi











