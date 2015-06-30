#!/bin/bash
#
# ----------------------------------------------------------------------



NUMBERS="$(seq 1 10)"
GL_JOB_ID='1435093342217'
GL_ITERS="$(seq 1 1 2)"
JOBS_PER_GL=2

parallel -X -j"$JOBS_PER_GL" echo {} ::: "$NUMBERS" | \
    parallel \
       -a - \
      --joblog /tmp/genlog.log \
      ./launch_job.sh "$GL_JOB_ID" \
                       '{2} {1}' \
    ::: "$GL_ITERS"
