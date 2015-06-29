#!/bin/bash
#
# ----------------------------------------------------------------------



NUMBERS="$(seq 1 99)"
GL_JOB_ID='1435093342217'
GL_ITERS="$(seq 1 1 1)"


# make temporary data directory
TMPDIR="$(mktemp -d -t genlog)"

parallel -X -j1 echo {} ::: "$GL_ITERS" | \
    parallel \
       -j10 -a - \
      --joblog /tmp/genlog.log \
      --files \
      ./launch_job.sh "$GL_JOB_ID" \
                       '{2} {1} >' "$TMPDIR"'/job_{#}.gl_iter_{2}.part.res' \
    ::: "$NUMBERS"


