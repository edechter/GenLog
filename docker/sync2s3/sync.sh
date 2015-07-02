#!/bin/bash 

usage="Usage: $0 [-i sync_interval] s3_bucket_url path."


# default interval in seconds
interval=10
while getopts ":i:" opt; do 
    case $opt in 
        i  ) interval=$OPTARG ;;
        \? ) echo $usage  
            exit ;;
    esac
done

shift $(($OPTIND - 1))
if [[ -z "$@" ]]; then 
    echo $usage
    exit 1
fi


if [[ -z "$1" ]]; then 
    echo $usage
    exit 1
else
    s3_bucket_url="$1"
fi


if [[ -z "$2" ]]; then 
    echo $usage
    exit 1
elif [[ ! -e "$2" ]]; then 
    echo "Error: cannot find $2."
    exit 1
else
    path="$2"
fi


while true; do
    aws s3 sync "$path" "$s3_bucket_url" 
    sleep $interval
done

