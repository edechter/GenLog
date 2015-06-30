#!/bin/bash
#
# ----------------------------------------------------------------------



JOB_ID=$(cat /dev/urandom | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)
echo "${JOB_ID}"
parallel --files ./launch_job.sh "${JOB_ID}" ::: "$(seq 1 2)"

