#!/bin/bash -x
#
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# ami: GENLOG
IMAGE_ID=ami-61cf330a

INSTANCE_TYPE=m1.large
USERDATA_SCRIPT=init_genlog.sh
SPOT_BID=0.05
INSTANCE_COUNT=1
KEY_PAIR=ec2

GENLOG_ARGS="$@"


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

GO="$CMD \
    ${IMAGE_ID} \
    --instance-count ${INSTANCE_COUNT} \
    --user-data-file ${USERDATA_SCRIPT} \
    --instance-type ${gINSTANCE_TYPE} \
    --key ${KEY_PAIR} \
    ${PRICE_OPT}"
echo $GO
$GO



    

