#!/bin/bash -xe
#
# ----------------------------------------------------------------------

# exec > >(tee /var/log/user-data.log|logger -t user-data -s 2>/dev/console) 2>&1

# set environment variables
AWS_ACCESS_KEY=AKIAI52M2XEJUPC3ZDYA
export AWS_ACCESS_KEY
AWS_SECRET_KEY=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
export AWS_SECRET_KEY

# install awscli
# sudo pip install awscli

# set up awscli config
mkdir -p ~/.aws
cat << EOF > ~/.aws/credentials
[default]
aws_access_key_id = ${AWS_ACCESS_KEY}
aws_secret_access_key = ${AWS_SECRET_KEY}
EOF
chmod 666 ~/.aws/credentials

cat << EOF > ~/.aws/config
[default]
region = us-east-1
EOF
chmod 666 ~/.aws/config

# add GitHub to known_hosts
ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts

# # add swipl repo
# # -y automatically responds yes to all queries
# # needed for non-interactive script
# sudo apt-add-repository -y ppa:swi-prolog/devel
# sudo apt-get -y update

# # install git
# sudo apt-get install -y git
# sudo apt-get install -y swi-prolog

# sudo apt-get -y upgrade

# ####################
# ## Get GenLog     ##
# ####################


S3_BUCKET=edechter.genlog
# get github ssh key from S3 bucket
aws s3 cp s3://edechter.genlog/.ssh ~/.ssh --recursive 
chmod 600 ~/.ssh/git/id_rsa

# clone GenLog repo from github
GENLOG_ROOT=~/GenLog
export GENLOG_ROOT
# delete repo if already exists
if [ -d ${GENLOG_ROOT} ]
then 
    echo "Found existing GenLog repo."
    echo "Deleting ${GENLOG_ROOT}."
    rm -rf ${GENLOG_ROOT}
fi

echo "Cloning GenLog Repo..."
git clone git@github.com:edechter/GenLog.git
echo "Done."

# source job script
GENLOG_JOB_SCRIPT=${GENLOG_ROOT}/ec2/job.sh
GENLOG_JOB_OUT=${GENLOG_ROOT}/ec2/job.out
chmod +x ${GENLOG_JOB_SCRIPT}
if [ ! -x ${GENLOG_JOB_SCRIPT} ]
then
    echo "Cannot execute GenLog ec2 job script: ${GENLOG_JOB_SCRIPT}" 
else
    echo "Executing GenLog ec2 job script: ${GENLOG_JOB_SCRIPT}..."
    echo "Time: $(date +%Y.%m.%d-%H.%M.%S)"
    bash -x ${GENLOG_JOB_SCRIPT}
    echo "GenLog ec2 job script, ${GENLOG_JOB_SCRIPT}, finished running."
    echo "Time: $(date +%Y.%m.%d-%H.%M.%S)"
fi

if [ ! -r GENLOG_JOB_OUT ]
then
    echo "Cannot find output of GenLog ec2 job script: ${GENLOG_JOB_OUT}"
else
    t=$( date "+%Y-%m-%d-%HH-%MM-%SS" )
    aws s3 cp ${GENLOG_JOB_OUT} "s3://${S3_BUCKET}/data/out_$t"
fi
