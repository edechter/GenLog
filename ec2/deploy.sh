#!/bin/bash 
#
# deploy.sh
# ----------------------------------------------------------------------
S3_DATA_URL='s3://edechter.genlog/data/data_job_id_1435093342217/'

parallel -X -j1 echo {} ::: $(seq 1 2) | \
    parallel -a - ./launch_ec2.sh $S3_DATA_URL/ovbem_gl_{2}.gl {1} \
    ::: $(seq -f "%04g" 1 1 2)




    

