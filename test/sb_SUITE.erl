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
        sb_interface_1_test/1
	]).

% SUITE callbacks

all() -> [{group, public}].

groups() -> [
	     {public, [shuffle], [sb_basic_test,
				  sb_metadata_test,
				  sb_interface_1_test
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
   sb:start_cluster(),
   sb:stop_cluster(),
   ok.

% check for metadata consistency
sb_metadata_test(_Config) ->
   % start
   sb:start_cluster(),
   {_, Cluster} = sbdbs:get_cluster_metadata(disk),
   % get the cluster nodes from the current node's metadata
   Nodes = Cluster#sys_meta.nodes,
   % get a list of each node metadata
   NList = [erpc:call(N,sbdbs,get_cluster_metadata,[disk]) || N <- Nodes],
   % a node's metadata must be equal to each other node's metadata
   ?assert(lists:all(fun(X) -> X =:= lists:nth(1, NList) end, NList), "Metadata must be equal to each other's node"),
   % end
   sb:stop_cluster(),
   ok.

% interface #1 general test
sb_interface_1_test(_Config) ->
   % start
   sb:start_cluster(),
   % create a new tuple space
%   sb:command({new, my_awesome_ts}),
   % populate the ts with random data
%   sb:command({out, my_awesome_ts, {a,b,c,d}}),
%   sb:command({out, my_awesome_ts, {ab,cd,ef}}),
%   sb:command({out, my_awesome_ts, {abc,def,ghi}}),
%   sb:command({out, my_awesome_ts, {abcd,efgh,ijkl}}),
   % read data
   ?assertMatch({ok, [{a,b,c,d}]}, sb:command({rd, my_awesome_ts, {a,b,c,d}})),
   ?assertMatch({ok, [{ab,cd,ef}]}, sb:command({rd, my_awesome_ts, {ab,cd,ef}})),
   ?assertMatch({ok, [{abc,def,ghi}]}, sb:command({rd, my_awesome_ts, {abc,def,ghi}})),
   ?assertMatch({ok, [{abcd,efgh,ijkl}]}, sb:command({rd, my_awesome_ts, {abcd,efgh,ijkl}})),
% TO DO
   % read data with cancellation
   %
   sb:stop_cluster(),
   ok.
