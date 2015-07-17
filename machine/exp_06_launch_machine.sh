#!/bin/bash -ex
#
# Launch an amazon ec2 docker machine instance for 
# exp_06
# ----------------------------------------------------------------------

# DRIVER=virtualbox
DRIVER=amazonec2

# NB: cannot use underscores in MACHINE_NAME
MACHINE_NAME=genlog-ec2

# ----------------------------------------------------------------------
source env_file

# docker pull ubuntu:latest


# if [[ "$DRIVER" = amazonec2 ]]; then 
#     driver_label=ec2
# elif [[ "$DRIVER" = virtualbox ]]; then 
#     driver_label=local
# else
#     driver_label="$DRIVER"
# fi

# # create swarm agents
# docker-machine create \
#     -d "$DRIVER" \
#     --amazonec2-access-key="$AWS_ACCESS_KEY" \
#     --amazonec2-secret-key="$AWS_SECRET_KEY" \
#     --amazonec2-instance-type=c3.large \
#     --amazonec2-vpc-id="$AWS_VPC_ID" \
#     "$MACHINE_NAME"

# connect to swarm-master
eval "$(docker-machine env  "${MACHINE_NAME}")"
if [[ $? -ne 0 ]]; then 
    echo "** Error: unable to connect to ${MACHINE_NAME}" 
    exit 1
fi

# create data and logs directory on master
docker-machine ssh "${MACHINE_NAME}" \
    'mkdir -p ~/data \
     && mkdir -p ~/logs'

if [[ $? -ne 0 ]]; then 
    echo "** Error: unable to create ~/data and ~/logs directory on ${MACHINE_NAME}" 
    exit 1
fi

echo "Done creating and launching $MACHINE_NAME."
exit 0


