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
	datafile_exists/2
	]).

% SUITE callbacks

all() -> [{group, public}].

groups() -> [
	     {public, [shuffle], [sb_basic_test,
				  sb_metadata_test,
				  sb_interface_1_test,
				  sb_interface_2_test,
				  sb_interface_3_test
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

   % start
   sbcli:start(),

   timer:sleep(2000),

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

   % start
   sbcli:start(),

   timer:sleep(2000),

   % create a new tuple space
   sbcli:new(adcc),

   % 
   ?assertMatch({err, timeout}, sbcli:in(adcc,{a,b,c,d}, 500)),
   ?assertMatch({err, timeout}, sbcli:rd(adcc,{a,b,c,d}, 500)),

   % stop
   sbcli:stop(),
   ok.


sb_interface_3_test(_Config) ->
   % create new  metadata
   sbcli:create_cluster_metadata(),

   % start
   sbcli:start(),

   timer:sleep(2000),

   TS = adcc,

   % create a new tuple space
   sbcli:new(TS),

   ?assertMatch(true, ?MODULE:datafile_exists(TS, node())),

   sbcli:addNode(TS, ra1@localhost),

   ?assertMatch(true, ?MODULE:datafile_exists(TS, ra1@localhost)),

   sbcli:removeNode(TS, ra1@localhost),

   ?assertMatch(false, ?MODULE:datafile_exists(TS, ra1@localhost)),

   % stop
   sbcli:stop(),
   ok.

datafile_exists(TSName, Node) ->
   {ok, CDHomeDir} = sbenv:get_cluster_env(cluster_datafiles_home_dir),                                                                     
   FName = atom_to_list(TSName) ++ atom_to_list('.dets'),                                                                                   
   FileName = string:join([CDHomeDir, atom_to_list(Node), FName], "/"),
   Result = file:read_file_info(FileName),
   case Result of
      {ok, _} -> true;
	    _ -> false
   end.
