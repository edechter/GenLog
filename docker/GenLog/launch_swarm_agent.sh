#!/bin/bash -ex
#
# Launch a swarm of docker machines
# ----------------------------------------------------------------------

SWARM_TOKEN="$1"
AGENT_ID="$2"
# DRIVER=virtualbox
DRIVER=amazonec2

# ----------------------------------------------------------------------

source env_file
export SWARM_TOKEN


if [[ "$DRIVER" = amazonec2 ]]; then 
    driver_label=ec2
elif [[ "$DRIVER" = virtualbox ]]; then 
    driver_label=local
else
    driver_label="$DRIVER"
fi

# create swarm agents

docker-machine create \
    -d "$DRIVER" \
    --amazonec2-access-key="$AWS_ACCESS_KEY" \
    --amazonec2-secret-key="$AWS_SECRET_KEY" \
    --amazonec2-instance-type=t2.small \
    --amazonec2-vpc-id="$AWS_VPC_ID" \
    --swarm \
    --swarm-discovery token://"$SWARM_TOKEN" \
    swarm-agent-"${driver_label}"-"$(printf "%03.0f" "$AGENT_ID")" \










