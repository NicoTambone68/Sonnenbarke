#!/bin/bash
DTTIME=`date +"%Y-%m-%d_%H%M%S"`
PWD=`pwd`

FILENAME="test_logs_${DTTIME}.tgz"
DIR="${PWD}/_build/test/logs" 

printf "${DTTIME}\n"
printf "${FILENAME}\n"
printf "${DIR}\n"

tar cvfz ${FILENAME} ${DIR}

printf "Done.\n"

