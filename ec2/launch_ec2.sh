#!/bin/bash -x
#
# ----------------------------------------------------------------------
set -x

IMAGE_ID=ami-765b3e1f
INSTANCE_TYPE=m1.small
USERDATA_SCRIPT_IN=setup_genlog.sh.in
USERDATA_SCRIPT=setup_genlog.sh
SPOT_BID=0.02
INSTANCE_COUNT=2
KEY_PAIR=ec2

sed "s-<AWS_ACCESS_KEY>-${AWS_ACCESS_KEY}-g; s-<AWS_SECRET_KEY>-${AWS_SECRET_KEY}-g;" \
    ${USERDATA_SCRIPT_IN} > ${USERDATA_SCRIPT}

ec2-request-spot-instances \
    ${IMAGE_ID} \
    --price ${SPOT_BID} \
    --instance-count ${INSTANCE_COUNT} \
    --user-data-file ${USERDATA_SCRIPT} \
    --instance-type ${INSTANCE_TYPE} \
    --key ${KEY_PAIR}


    

