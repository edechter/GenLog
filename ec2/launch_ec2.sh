#!/bin/bash -x
#
# ----------------------------------------------------------------------
set -x

# genlog_ami
IMAGE_ID=ami-2b3ac740

INSTANCE_TYPE=m1.small
USERDATA_SCRIPT=run_genlog.sh
SPOT_BID=0.02
INSTANCE_COUNT=1
KEY_PAIR=ec2
# Should we use an on-demand instance (0 for spot instance)
ON_DEMAND=1


if [ $ON_DEMAND == 1 ] 
then
    CMD=ec2-run-instances
    PRICE_OPT=""
else
    CMD=ec2-request-spot-instances
    PRICE_OPT="--price $SPOT_BID"
fi

$CMD \
    ${IMAGE_ID} \
    --instance-count ${INSTANCE_COUNT} \
    --user-data-file ${USERDATA_SCRIPT} \
    --instance-type ${INSTANCE_TYPE} \
    --key ${KEY_PAIR} \
    ${PRICE_OPT}




    

