printf "Starting automatic tests\n"
printf "Be sure to have ra1, ra2, ra3 started in other shells, but not ra4\n"
#printf "Starting ra1@localhost in background...\n"
#nohup rebar3 shell --sname ra1@localhost > /dev/null 2>&1 &
#pid_ra1=$!

#sleep 2s

#printf "Starting ra2@localhost in background...\n"
#nohup rebar3 shell --sname ra2@localhost > /dev/null 2>&1 &
#pid_ra2=$!

#sleep 2s

#printf "Starting ra3@localhost in background...\n"
#nohup rebar3 shell --sname ra3@localhost > /dev/null 2>&1 &
#pid_ra3=$!

#sleep 4s

printf "Clearing _build/test\n"
rm -rf _build/test

sleep 2s

printf "Running test as ra4@localhost\n"
rebar3 ct --sname=ra4@localhost --suite=test/sb_SUITE

#printf "Test is done. Now killing the background processes\n"
#kill -9 $pid_ra1
#kill -9 $pid_ra2
#kill -9 $pid_ra3

printf "Done. Cheers\n"

