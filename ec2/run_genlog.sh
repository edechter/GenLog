#!/bin/bash 
#
# run_genlog.sh
# ----------------------------------------------------------------------

# source job script
GENLOG_ARGS="$@"
GENLOG_JOB_SCRIPT=${GENLOG_ROOT}/ec2/job.sh
GENLOG_JOB_OUT=${GENLOG_ROOT}/ec2/job.out
GENLOG_JOB_LOG=${GENLOG_ROOT}/ec2/job.log
GENLOG_JOB_ID=$( date +%s%3N ) # milliseconds since epoch
GENLOG_JOB_DATA_PRE="data_job_id"
GENLOG_JOB_DATA_PATH=${GENLOG_EXPERIMENT_DATA_DIR}${GENLOG_JOB_DATA_PRE}_${GENLOG_JOB_ID}


LOG_PRE="GenLog Job ${GENLOG_JOB_ID}: "q
CMD="chmod +x ${GENLOG_JOB_SCRIPT}"
$CMD
chmod +x ${GENLOG_JOB_SCRIPT}
if [ ! -x ${GENLOG_JOB_SCRIPT} ]
then
    echo "$LOG_PRE: Cannot execute GenLog ec2 job script: ${GENLOG_JOB_SCRIPT}" 
else
    echo "$LOG_PRE: making data dir (if it does not already exist)..."
    mkdir -p "$GENLOG_JOB_DATA_DIR"

    echo "$LOG_PRE: Executing GenLog ec2 job script: ${GENLOG_JOB_SCRIPT}..."
    echo "$LOG_PRE: Time: $(date +%Y.%m.%d-%H.%M.%S)"
    bash -x ${GENLOG_JOB_SCRIPT} ${GENLOG_JOB_DATA_PATH} ${GENLOG_ARGS} &>> ${GENLOG_JOB_LOG} &  
    JOB_PID=$!
fi

# If this script is killed, kill the `cp'.
trap "kill $pid 2> /dev/null" EXIT

S3_BUCKET=edechter.genlog

# Transfer data to S3
S3_DATA_DIR=data
S3_DATA_FILENAME="${GENLOG_JOB_DATA_PRE}_${GENLOG_JOB_ID}"
S3_DATA_URL="s3://${S3_BUCKET}/${S3_DATA_DIR}/${S3_DATA_FILENAME}"

S3_LOG_DIR="logs/jobs"
S3_LOG_PRE="log_job_id_"
S3_LOG_FILENAME="${S3_LOG_PRE}_${GENLOG_JOB_ID}"
S3_LOG_URL="s3://${S3_BUCKET}/${S3_LOG_DIR}/${S3_LOG_FILENAME}"

# how often to sync data dir with s3 bucket (seconds)
S3_SYNC_INTERVAL=5 


# While copy is running...
while kill -0 $JOB_PID 2> /dev/null; do
    sleep $S3_SYNC_INTERVAL
    echo "$LOG_PRE: Compressing data files..."
    CMD="gzip ${GENLOG_JOB_DATA_PATH}/*.gl"
    echo $CMD
    $CMD
    if [ $? -eq 0 ]; then
        echo "$LOG_PRE: compression succeeded."

        echo "$LOG_PRE: Syncing experiment data with S3 bucket..."
        CMD="aws s3 sync ${GENLOG_JOB_DATA_PATH} ${S3_DATA_URL}"
        echo $CMD
        $CMD
        if [ $? -eq 0 ]; then
            echo "$LOG_PRE: Data sync succeeded."

            echo "$LOG_PRE: Clean up local data."
            CMD="rm -fr ${GENLOG_JOB_DATA_PATH}/*"
            echo $CMD
            $CMD
            if [ $? -eq 0 ]; then
                echo "$LOG_PRE: Clean up succeeded. Files removed"
            else
                echo "$LOG_PRE: Clean up failed."
            fi
        else
            echo "$LOG_PRE: Data sync failed."
        fi

    else
        echo "$LOG_PRE: compression failed."
    fi
    
done

echo "$LOG_PRE: GenLog ec2 job script, ${GENLOG_JOB_SCRIPT}, finished running."
echo "$LOG_PRE: Time: $(date +%Y.%m.%d-%H.%M.%S)"

# upload job.log to S3
if [ ! -r ${GENLOG_JOB_LOG} ]
then
    echo "$LOG_PRE: Cannot find log file: ${GENLOG_JOB_LOG}"
        
else
    echo "$LOG_PRE: Uploading log file  to S3 bucket..."
    CMD="aws s3 cp ${GENLOG_JOB_LOG} ${S3_LOG_URL}"
    echo $CMD
    $CMD
    if [ $? -eq 0 ]; then
        echo "$LOG_PRE: Data transfer succeeded."
    else
        echo "$LOG_PRE: Data transfer failed."
   fi
fi

# Disable the trap on a normal exit.
trap - EXIT
