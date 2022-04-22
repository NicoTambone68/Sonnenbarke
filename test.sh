printf "Starting automatized tests\n"
printf "Be sure to have separate shells for ra1, ra2, ra3 and NOT ra4 \n"

printf "Checking cluster nodes...\n"

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

export FQDN=`hostname --fqdn`

printf "Clearing _build/test\n"

rm -rf _build/test

sleep 2s

export nodename="ra4@${FQDN}"

printf "Running test as %s\n" $nodename

rebar3 ct --name=$nodename --suite=test/sb_SUITE


printf "Done. Cheers\n"

