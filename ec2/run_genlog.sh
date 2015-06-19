#!/bin/bash -x
# run_genlog.sh
# 
# ----------------------------------------------------------------------

# set up user-data logger
exec > >(tee /var/log/user-data.log|logger -t user-data -s 2>/dev/console) 2>&1

# change directory to the root of the GenLog project
cd $GENLOG_ROOT

# pull most recent changes
echo "Pulling from GenLog repo..."
git pull
echo "Done."

# source job script
GENLOG_JOB_SCRIPT=${GENLOG_ROOT}/ec2/job.sh
GENLOG_JOB_OUT=${GENLOG_ROOT}/ec2/job.out
GENLOG_JOB_LOG=${GENLOG_ROOT}/ec2/job.log
GENLOG_JOB_ID=$( date +%s%3N ) # milliseconds since epoch
LOG_PRE="GenLog Job ${GENLOG_JOB_ID}: "
CMD="chmod +x ${GENLOG_JOB_SCRIPT}"
$CMD
chmod +x ${GENLOG_JOB_SCRIPT}
if [ ! -x ${GENLOG_JOB_SCRIPT} ]
then
    echo "$LOG_PRE: Cannot execute GenLog ec2 job script: ${GENLOG_JOB_SCRIPT}" 
else
    echo "$LOG_PRE: Executing GenLog ec2 job script: ${GENLOG_JOB_SCRIPT}..."
    echo "$LOG_PRE: Time: $(date +%Y.%m.%d-%H.%M.%S)"
    bash -x ${GENLOG_JOB_SCRIPT} 2> ${GENLOG_JOB_LOG}
    echo "$LOG_PRE: GenLog ec2 job script, ${GENLOG_JOB_SCRIPT}, finished running."
    echo "$LOG_PRE: Time: $(date +%Y.%m.%d-%H.%M.%S)"
fi

# Transfer data to S3
S3_DATA_DIR=data
S3_DATA_PRE="out_job_id_"
S3_DATA_FILENAME="${S3_DATA_PRE}_${GENLOG_JOB_ID}"
S3_DATA_URL="s3://${S3_BUCKET}/${S3_DATA_DIR}/${S3_DATA_FILENAME}"

S3_LOG_DIR="logs/jobs"
S3_LOG_PRE="log_job_id_"
S3_LOG_FILENAME="${S3_LOG_PRE}_${GENLOG_JOB_ID}"
S3_LOG_URL="s3://${S3_BUCKET}/${S3_LOG_DIR}/${S3_LOG_FILENAME}"

S3_LOG_URL="s3://${S3_BUCKET}/${S3_DATA_PATH}/${S3_DATA_FILENAME}"


# upload job.out to S3
if [ ! -r ${GENLOG_JOB_OUT} ]
then
    echo "$LOG_PRE: Cannot find output of GenLog ec2 job script: ${GENLOG_JOB_OUT}"
        
else
    echo "$LOG_PRE: Uploading job.out data to S3 bucket..."
    aws s3 cp ${GENLOG_JOB_OUT} ${S3_DATA_URL}
    if [ $? -eq 0 ]; then
        echo "$LOG_PRE: Data transfer succeeded."
    else
        echo "$LOG_PRE: Data transfer failed."
    fi
fi

# upload job.log to S3
if [ ! -r ${GENLOG_JOB_LOG} ]
then
    echo "$LOG_PRE: Cannot find log file: ${GENLOG_JOB_LOG}"
        
else
    echo "$LOG_PRE: Uploading log file  to S3 bucket..."
    aws s3 cp ${GENLOG_JOB_LOG} ${S3_LOG_URL}
    if [ $? -eq 0 ]; then
        echo "$LOG_PRE: Data transfer succeeded."
    else
        echo "$LOG_PRE: Data transfer failed."
   fi
fi
