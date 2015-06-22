#!/bin/bash -x
#
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# ami: GENLOG
IMAGE_ID=ami-61cf330a

INSTANCE_TYPE=m1.small
USERDATA_SCRIPT=init_genlog.sh
SPOT_BID=0.02
INSTANCE_COUNT=2
KEY_PAIR=ec2

# Should we use an on-demand instance (0 for spot instance)
ON_DEMAND=0

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------


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




    

