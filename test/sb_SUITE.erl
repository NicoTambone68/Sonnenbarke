-module(sb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("sys_meta.hrl").

-export([
    all/0,
    end_per_suite/2,
    groups/0,
    init_per_suite/2
]).

-export([ 
	sb_basic_test/1,
	sb_metadata_test/1,
        sb_interface_1_test/1,
        sb_interface_2_test/1,
        sb_interface_3_test/1,
        sb_node_replication_test/1,
	datafile_exists/2
	]).

% SUITE callbacks

all() -> [{group, public}].

groups() -> [
	     {public, [shuffle], [
%				  sb_basic_test,
%				  sb_metadata_test,
%				  sb_interface_1_test,
%				  sb_interface_2_test,
%				  sb_interface_3_testi,
                                  sb_node_replication_test
				 ]}
	    ].


init_per_suite(public, Config) ->
	Config;
init_per_suite(_, Config) -> Config.


end_per_suite(public, _) ->
	ok;
end_per_suite(_, _) -> ok.

% ////
%
% public group tests

sb_basic_test(_Config) ->
   sbcli:start(),
   sbcli:stop(),
   ok.

% check for metadata consistency
sb_metadata_test(_Config) ->
   % start
   sbcli:start(),

   timer:sleep(2000),

   % get cluster nodes from env 
   {ok,L} = application:get_env(system_metadata, cluster),
   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, L),
   
   % get a list of each node metadata
   % NList = [erpc:call(N,sbdbs,get_cluster_metadata,[disk]) || N <- Nodes],
   NList = [erpc:call(N, sbcli, metadata, []) || N <- Nodes],

   % a node's metadata must be equal to each other node's metadata
   ?assert(lists:all(fun(X) -> X =:= lists:nth(1, NList) end, NList), "Metadata must be equal to each other's node"),
   % end
   sbcli:stop(),
   ok.

% interface #1 general test
sb_interface_1_test(_Config) ->

   % create new  metadata
   sbcli:create_cluster_metadata(),
   
   timer:sleep(2000),

   % start
   sbcli:start(),


   % create a new tuple space
   sbcli:new(adcc),

   % populate the ts with random data
   sbcli:out(adcc, {a,b,c,d}),
   sbcli:out(adcc, {ab,cd,ef}),
   sbcli:out(adcc, {abc,def,ghi}),
   sbcli:out(adcc, {abcd,efgh,ijkl}),

   % read data
   ?assertMatch([{a,b,c,d}], sbcli:rd(adcc,{a,b,c,d})),
   ?assertMatch([{ab,cd,ef}], sbcli:rd(adcc, {ab,cd,ef})),
   ?assertMatch([{abc,def,ghi}], sbcli:rd(adcc, {abc,def,ghi})),
   ?assertMatch([{abcd,efgh,ijkl}], sbcli:rd(adcc, {abcd,efgh,ijkl})),

   % read data with cancellation
   ?assertMatch([{a,b,c,d}], sbcli:in(adcc,{a,b,c,d})),
   ?assertMatch([{ab,cd,ef}], sbcli:in(adcc, {ab,cd,ef})),
   ?assertMatch([{abc,def,ghi}], sbcli:in(adcc, {abc,def,ghi})),
   ?assertMatch([{abcd,efgh,ijkl}], sbcli:in(adcc, {abcd,efgh,ijkl})),

   % stop
   sbcli:stop(),
   ok.


%
sb_interface_2_test(_Config) ->
   
   % create new  metadata
   sbcli:create_cluster_metadata(),
   
   timer:sleep(2000),

   % start
   sbcli:start(),

   % create a new tuple space
   sbcli:new(adcc),

   % 
   ?assertMatch({err, timeout}, sbcli:in(adcc,{a,b,c,d}, 500)),
   ?assertMatch({err, timeout}, sbcli:rd(adcc,{a,b,c,d}, 500)),

   % stop
   sbcli:stop(),
   ok.


sb_interface_3_test(_Config) ->
   % Create new  metadata
   sbcli:create_cluster_metadata(),
   
   timer:sleep(1000),

   % Start the cluster
   sbcli:start(),

   timer:sleep(1000),

   {ok, Ln} = application:get_env(system_metadata, cluster),
   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, Ln),

   TS = adcc,

   NumberOfTuples = 100,

   % Create a new tuple space
   sbcli:new(TS),

   % Replicate the tuple space on all of the cluster nodes
   [sbcli:addNode(TS, N) || N <- Nodes],
   
   timer:sleep(1000),

   % The TS's datafile now must exists on all nodes
   [?assertMatch(true, ?MODULE:datafile_exists(TS, N)) || N <- Nodes],

   % Generate a list of random tuples   
   ListOfTuples = [{Idx, rand:uniform(1000)} || Idx <- lists:seq(1, NumberOfTuples)],

   % Insert the tuples on the TS
   [sbcli:out(TS, T) || T <- ListOfTuples],
   
   % Allow some time to flush the buffers
   timer:sleep(1000),

   % Check that the stored tuples are the very same we have inserted before, on all nodes
   % [?assertMatch(true, lists:reverse(ListOfTuples) =:= sbcli:select_all(TS, N)) ||  N <- Nodes],

   % An alternative way to do the above. More reliable. Stored data on the TS must be the same on all nodes replica of the TS
   [[?assertMatch(true, lists:member(lists:nth(Elem, ListOfTuples), sbcli:select_all(TS, N))) || Elem <- lists:seq(1, NumberOfTuples)] || N <- Nodes ],

   % To debug the test after the debug of the debug
   % [ct:print("Node: ~p Data: ~p~n", [N, sbcli:select_all(TS, N)]) || N <- Nodes],

   % Now remove the TS from all the nodes
   [sbcli:removeNode(TS, N) || N <- Nodes],
   
   timer:sleep(1000),

   % The TS datafile must be gone on every node
   [?assertMatch(false, ?MODULE:datafile_exists(TS, N)) || N <- Nodes],

   % Done. Stop the cluster 
   sbcli:stop(),

   ok.



sb_node_replication_test(_Config) ->
   % Create new  metadata
   sbcli:create_cluster_metadata(),
   
   timer:sleep(1000),

   % Start the cluster
   sbcli:start(),

   timer:sleep(1000),

   {ok, Ln} = application:get_env(system_metadata, cluster),
   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, Ln),

   TS = adcc_node_repl_test,

   NumberOfTuples = 100,

   % Create a new tuple space
   sbcli:new(TS),

   Node = get_random_node(Nodes, node()),

   % List of the replicated Nodes
   ReplNodes = [Node, node()],
   
   % List of the not replicated Nodes
   NotReplNodes = [N || N <- Nodes, (lists:member(N, ReplNodes) =:= false)],

   % Replicate the tuple space on another random node 
   sbcli:addNode(TS, Node),

   timer:sleep(1000),

   % The TS's datafile now must exists on all the replicated nodes
   [?assertMatch(true, ?MODULE:datafile_exists(TS, N)) || N <- ReplNodes],

   % The TS's datafile now must NOT exists on all the other nodes
   [?assertMatch(false, ?MODULE:datafile_exists(TS, N)) || N <- NotReplNodes],

   % Generate a list of random tuples   
   ListOfTuples = [{Idx, rand:uniform(1000)} || Idx <- lists:seq(1, NumberOfTuples)],

   % Insert the tuples on the TS
   [sbcli:out(TS, T) || T <- ListOfTuples],
   
   % Allow some time to flush the buffers
   timer:sleep(1000),

   % Check data integrity. Stored data on the TS must be the same on all nodes replica of the TS
   [[?assertMatch(true, lists:member(lists:nth(Elem, ListOfTuples), sbcli:select_all(TS, N))) 
                  || Elem <- lists:seq(1, NumberOfTuples)] || N <- ReplNodes],
  

   % Now remove the TS from all the replicated nodes
   [sbcli:removeNode(TS, N) || N <- ReplNodes],
   
   timer:sleep(1000),

   % The TS datafile must be gone on every node
   [?assertMatch(false, ?MODULE:datafile_exists(TS, N)) || N <- Nodes],

   % Done. Stop the cluster 
   sbcli:stop(),

   ok.


% selects from the list Nodes a random node different than Node
get_random_node(Nodes, Node) ->
   Index = rand:uniform(length(Nodes)),
   case N = lists:nth(Index, Nodes) of
      Node -> get_random_node(Nodes, Node);
	_  -> N
   end.



datafile_exists(TSName, Node) ->
   {ok, CDHomeDir} = sbenv:get_cluster_env(cluster_datafiles_home_dir),                                                                     
   FName = atom_to_list(TSName) ++ atom_to_list('.dets'),                                                                                   
   FileName = string:join([CDHomeDir, atom_to_list(Node), FName], "/"),
   filelib:is_regular(FileName).
