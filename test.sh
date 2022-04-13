printf "Starting automatic tests\n"
printf "Be sure to have ra1, ra2, ra3 started in other shells, but not ra4\n"

export FQDN=`hostname --fqdn`

printf "Clearing _build/test\n"

rm -rf _build/test

sleep 2s

export hostname="ra4@${FQDN}"

printf "Running test as %s\n" $hostname

rebar3 ct --name=$hostname --suite=test/sb_SUITE


printf "Done. Cheers\n"

