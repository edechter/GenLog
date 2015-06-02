#!/bin/bash -ex
#
# ----------------------------------------------------------------------
set -x

IMAGE_ID=ami-765b3e1f
INSTANCE_TYPE=m1.small
USERDATA_SCRIPT=setup_genlog.sh
SPOT_BID=0.02
INSTANCE_COUNT=1
KEY_PAIR=eyal_ec2

ec2-request-spot-instances \
    ${IMAGE_ID} \
    --price ${SPOT_BID} \
    --instance-count ${INSTANCE_COUNT} \
    --user-data-file ${USERDATA_SCRIPT} \
    --instance-type ${INSTANCE_TYPE} \
    --key ${KEY_PAIR}


    

