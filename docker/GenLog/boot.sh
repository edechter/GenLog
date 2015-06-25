#!/bin/bash -x
#
# run as root at docker container startup

# create aws config file for genlog 
mkdir -p /home/genlog/.aws
touch /home/genlog/.aws/config
chmod 600 /home/genlog/.aws/config
echo "[default]" >> /home/genlog/.aws/config
echo "aws_access_key_id = $AWS_ACCESS_KEY" >> /home/genlog/.aws/config
echo "aws_secret_acces_key = $AWS_SECRET_KEY" >> /home/genlog/.aws/config
echo "region = us-east-1" >> /home/genlog/.aws/config
chown genlog /home/genlog/.aws/config


