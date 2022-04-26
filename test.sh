#!/bin/bash
export FQDN=`hostname --fqdn`

printf "Starting automatized tests\n"
printf "..........................\n"
printf "Launching cluster nodes\n"


# Note: we are going to make use of the terminal multiplexer utility screen
# we'll instantiate the shells without attaching to them
# we just need the nodes to be up
screen -d -m bash -c "rebar3 shell --name ra1@$FQDN"
screen -d -m bash -c "rebar3 shell --name ra2@$FQDN"
screen -d -m bash -c "rebar3 shell --name ra3@$FQDN"


printf "Checking the cluster nodes...\n"

RA1=`ps -ef | grep rebar3 | awk '/ra1/' | wc -l`
RA2=`ps -ef | grep rebar3 | awk '/ra2/' | wc -l`
RA3=`ps -ef | grep rebar3 | awk '/ra3/' | wc -l`
RA4=`ps -ef | grep rebar3 | awk '/ra4/' | wc -l`

CHK=0

if [ $RA1 -lt 1 ]; then
	printf "node ra1 must be ready!\n"
	CHK=-1
fi

if [ $RA2 -lt 1 ]; then
	printf "node ra2 must be ready!\n"
	CHK=-1
fi

if [ $RA3 -lt 1 ]; then
	printf "node ra3 must be ready!\n"
	CHK=-1
fi

if [ $RA4 -ge 1 ]; then
	printf "node ra4 must NOT be ready!\n"
	CHK=-1
fi

if [ $CHK -lt 0 ]; then
  printf "ERROR: One or more nodes not available. Please check!\n"	
  exit -1
fi

printf "Nodes ok, now starting tests...\n"


printf "Clearing _build/test\n"

rm -rf _build/test

sleep 2s

export nodename="ra4@${FQDN}"

printf "Running test as %s\n" $nodename

# launch the automated tests
rebar3 ct --name=$nodename --suite=test/sb_SUITE

printf "Closing the cluster nodes\n"

sleep 1s

# kill the processes of all the cluster nodes open on the 
# terminal multiplexer utility
kill $(ps -ef|grep rebar3|grep SCREEN|awk '{print $2}')

printf "Done. Cheers\n"
