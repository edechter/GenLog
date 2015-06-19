#!/bin/bash 
set -x

echo "job.sh"
RUNNER_PATH="${GENLOG_ROOT}/experiments/scripts/learn_number_morph/runner.pl"
EXEC_PATH="${GENLOG_ROOT}/src/run.pl"

if [ -z "$2" ] 
then
    echo "JOB.sh: ** No data path supplied in second argument **"
    exit(1)
else
    DATA_PATH=$2
fi

if [ ! -x ${EXEC_PATH} ] 
then
    echo "JOB.SH: ** Cannot run $EXEC_PATH"
else
    $EXEC_PATH $RUNNER_PATH $DATA_PATH > ${GENLOG_ROOT}/ec2/job.out
fi

