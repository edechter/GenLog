#!/bin/bash
#
# ----------------------------------------------------------------------



NUMBERS="$(seq 1 2)"
GL_JOB_ID='1435093342217'
GL_ITERS="$(seq 1 1 1)"


parallel -X -j1 echo {} ::: "$GL_ITERS" | \
    parallel \
    --dry-run \
       -j10 -a - \
      --joblog /tmp/genlog.log \
      ./launch_job.sh "$GL_JOB_ID" \
                       '{2} {1}' \
    ::: "$NUMBERS"
