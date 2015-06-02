#!/bin/sh
echo "          Installing SWI Prolog Devel"
sudo apt-add-repository ppa:swi-prolog/devel
sudo apt-get update
sudo apt-get install swi-prolog
