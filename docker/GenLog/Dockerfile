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
RUN apt-get -y install software-properties-common
RUN apt-add-repository ppa:swi-prolog/devel
RUN apt-get -y update
RUN apt-get -y install swi-prolog

# install git
RUN apt-get -y install git

# install wget
RUN apt-get -y install wget

# install gsl
# RUN apt-get -y install gsl-bin
RUN apt-get -y install libgsl0-dev

# install r
RUN apt-get -y install r-base

# install matrixStats r package
RUN wget http://cran.r-project.org/src/contrib/matrixStats_0.14.2.tar.gz
RUN R CMD INSTALL matrixStats_0.14.2.tar.gz

# install aws-cli
RUN apt-get install -y python-pip
RUN pip install awscli

# create user and group genlog
RUN useradd \
    --home /home/genlog \
    --home /home/genlog \
    --create-home \
    --shell /bin/bash \
    --groups sudo genlog

RUN echo "%sudo ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# ADD ./boot.sh .
# CMD ./boot.sh

# change to genlog user
WORKDIR /home/genlog

ENV HOME /home/genlog
ENV CC gcc

ADD ./boot.sh ./
RUN sudo chown genlog ./boot.sh
RUN chmod +x ./boot.sh

USER genlog

ENTRYPOINT /home/genlog/boot.sh
