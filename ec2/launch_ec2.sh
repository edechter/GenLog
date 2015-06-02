#!/bin/bash -x
#
# ----------------------------------------------------------------------
set -x

IMAGE_ID=ami-765b3e1f
INSTANCE_TYPE=m1.small
USERDATA_SCRIPT_IN=setup_genlog.sh.in
USERDATA_SCRIPT=setup_genlog.sh
SPOT_BID=0.02
INSTANCE_COUNT=1
KEY_PAIR=ec2
# Should we use an on-demand instance (0 for spot instance)
ON_DEMAND=1

SUBST="s-<AWS_ACCESS_KEY>-${AWS_ACCESS_KEY}-g;" 
SUBST="$SUBST s-<AWS_SECRET_KEY>-${AWS_SECRET_KEY}-g;"
SUBST="$SUBST s-<GMAIL_USER>-${GMAIL_USER}-g;"
SUBST="$SUBST s-<GMAIL_PASSWORD>-${GMAIL_PASSWORD}-g;"
sed $SUBST
    ${USERDATA_SCRIPT_IN} > ${USERDATA_SCRIPT}


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


    

