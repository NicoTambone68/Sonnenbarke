% Tuple-space management
-module(sbts).
-behaviour(gen_server).
-include("sys_meta.hrl").

-export([
	new/1,
	in/2,
	rd/2,
	out/2,

	in/3,
	rd/3,

	addNode/2,
	removeNode/2,
	nodes/1
	
	]).

-define(SYSTEM, #sys_meta).

%TO DO: implement
%
% TO DO: First time you create a TS the default associated node is 
% the one requesting the new TS. Other nodes can be added later on
% with the given API
new(Name) -> 
   % register the new TS in the system's metadata table
   % get current metadata
   {_, ClusterMetadata} = sbsystem:get_cluster_metadata(),
   % get current leader node
   {_, Leader} = ra_leaderboard:lookup_leader(ClusterMetadata?SYSTEM.name),
   % append the new Tuple Space to the list. Note that the new name is
   % associated with the current leader node by default
   TS = [{Name, [Leader]} | ClusterMetadata?SYSTEM.ts],
   % TS = lists:append(ClusterMetadata?SYSTEM.ts, {Name, [Leader]}),
   % Store the updated metadata on a peg variable
   CMetaUpdated = ClusterMetadata?SYSTEM{ts = TS},
   % Save the updated metadata to ram and disk
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   ok.

in(TS, Pattern) -> ok.

rd(TS, Pattern) -> 
   sbdbs:match_ts(TS, Pattern).

out(TS, Tuple) -> 
  {R1, R2} = sbdbs:insert_table(TS, Tuple).

% Interface 2/3
in(TS, Pattern, Timeout) -> ok.
rd(TS, Pattern, Timeout) -> ok.

% Interface 3/3
addNode(TS, Node) -> ok.
removeNode(TS, Node) -> ok.
nodes(TS) -> ok.

%gen_server's callbacks
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) -> ok.

stop() ->
   gen_server:cast(?MODULE, stop).	
	
handle_cast(stop, State) -> ok.
