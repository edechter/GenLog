#!/bin/bash
#
# ----------------------------------------------------------------------



NUMBERS="$(seq 1 5)"
GL_JOB_ID='1435093333723'
GL_ITERS="$(seq 1 20 50)"
JOBS_PER_GL=2

JOB_ID=$(cat /dev/urandom | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)

echo "${JOB_ID}"

parallel -X -j"$JOBS_PER_GL" echo {} ::: "$NUMBERS" | \
    parallel \
       -a - \
      --joblog /tmp/genlog.log \
      ./launch_job.sh "$JOB_ID" \
                    "$GL_JOB_ID" \
                       '{2} {1}' \
    ::: "$GL_ITERS"
