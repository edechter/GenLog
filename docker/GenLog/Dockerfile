############################################################
# Dockerfile to build MongoDB container images
# Based on Ubuntu
############################################################

# Set the base image to Ubuntu
FROM ubuntu

# File Author / Maintainer
MAINTAINER Eyal Dechter

############################################################
# Start building image here
############################################################

# source env_file
ADD ./env_file .
RUN sh env_file

# Update the repository sources list
RUN apt-get -y install software-properties-common \
    && apt-add-repository ppa:swi-prolog/devel

RUN apt-get -y update \
    && apt-get -y install swi-prolog \
    && apt-get -y install git \
    && apt-get -y install wget \
    && apt-get -y install libgsl0-dev \   
    && apt-get -y install r-base \
    && apt-get install -y python-pip

# install matrixStats r package
RUN wget http://cran.r-project.org/src/contrib/matrixStats_0.14.2.tar.gz \
    && R CMD INSTALL matrixStats_0.14.2.tar.gz

# install aws-cli
RUN pip install awscli

# create user and group genlog
RUN useradd \
    --home /home/genlog \
    --home /home/genlog \
    --create-home \
    --shell /bin/bash \
    --groups sudo genlog

RUN echo "%sudo ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# change to genlog user
WORKDIR /home/genlog

ENV HOME /home/genlog
ENV CC gcc

# install real package for swipl
RUN sudo chmod 777 /usr/lib/swi-prolog/
RUN swipl -g "pack_install(real, [interactive(false), quiet(true)]), halt."

ADD ./boot.sh ./
RUN sudo chown genlog ./boot.sh
RUN chmod +x ./boot.sh

USER genlog

RUN mkdir -p /home/genlog/data \
    && chown genlog /home/genlog/data
RUN mkdir -p /home/genlog/logs \
    && chown genlog /home/genlog/logs

ENTRYPOINT ["/home/genlog/boot.sh"]
