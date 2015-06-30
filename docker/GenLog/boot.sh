#!/bin/bash -ex

echo this is a check

# create aws config file for genlog 
mkdir -p /home/genlog/.aws
touch /home/genlog/.aws/config
chmod 600 /home/genlog/.aws/config
echo "[default]" >> /home/genlog/.aws/config
echo "aws_access_key_id = $AWS_ACCESS_KEY" >> /home/genlog/.aws/config
echo "aws_secret_access_key = $AWS_SECRET_KEY" >> /home/genlog/.aws/config
echo "region = us-east-1" >> /home/genlog/.aws/config
chown genlog /home/genlog/.aws/config

# get genlog 
S3_BUCKET=edechter.genlog

# get github ssh key from S3 bucket
aws s3 cp s3://edechter.genlog/.ssh/ $HOME/.ssh/ --recursive
sudo echo $SSH_KEY > ~/.ssh/authorized_keys
sudo chown genlog ~/.ssh
sudo chmod 700 ~/.ssh
sudo chown genlog ~/.ssh/*
sudo chmod 600 ~/.ssh/*
sudo chown genlog ~/.ssh/git
sudo chmod 700 ~/.ssh/git
sudo chown genlog ~/.ssh/git/*
sudo chmod 600 ~/.ssh/git/*

# add GitHub to known_hosts
touch  ~/.ssh/known_hosts
chown genlog  ~/.ssh/known_hosts
chmod 660  ~/.ssh/known_hosts
ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts

GENLOG_ROOT=~/GenLog
export GENLOG_ROOT

# echo "Cloning GenLog Repo..."
git clone git@github.com:edechter/GenLog.git $GENLOG_ROOT

cd $GENLOG_ROOT

# build GenLog
export CPPFLAGS="-I/usr/lib/swi-prolog/include/ -I/usr/include/"
export LDGLAGS="-L/usr/lib/"
make lib

exec "$@"
