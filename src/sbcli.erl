%%
%%
%%
%% @doc This module implements a minimal CLI for interacting with the Tuple Space.
%% It's mainly a wrapper for the sb module's functions 

-module(sbcli).

-export([
	 % Client functions
%	 connect/0,
	 start/0,
	 stop/0,
%	 stop_node/1,
	 metadata/0,
	 create_cluster_metadata/0,
	 set_leader/1,

	 % Interface 1
	 new/1,
	 in/2,
	 rd/2,
	 out/2,

	 % Interface 2
	 in/3,
	 rd/3,

         % Interface 3
	 addNode/2,
	 removeNode/2,
	 nodes/1

       ]).

% Client utilities
% TO DO: check cluster status
% ok when connect is successf.
%connect() -> {ok}.

%% @doc Starts the cluster
%%
%% @param none
%%
%% @returns term()
%%
%% @end
-spec start() -> term().
start() ->
   sb:start_cluster().

%% @doc Stops the cluster
%%
%% @param none
%%
%% @returns term()
%%
%% @end
-spec stop() -> term().
stop() ->
   sb:stop_cluster().	

%% @doc 
%%
%% @param 
%%
%% @returns
%%
%% @end

%stop_node(Node) ->
%   erpc:call(Node, sb, stop_node, [], 5000).

%% @doc gets current metadata  
%%
%% @param none
%%
%% @returns Metadata = term()
%%
%% @end
-spec metadata() -> term().
metadata() ->
   sb:get_cluster_metadata(ram).

%% @doc Set the cluster's current leader to Node
%%
%% @param Node = atom()
%%
%% @returns term()
%%
%% @end
-spec set_leader(atom()) -> term().
set_leader(Node) ->
   ClusterName = sbsystem:get_cluster_name(),
   {_, Leader} = ra_leaderboard:lookup_leader(ClusterName),
   ra:transfer_leadership({ClusterName, Leader}, {ClusterName, Node}).

%% @doc Initialize new metadata on all nodes.
%% Based on sys.config. Creates directories if not exist.
%% WARNING! overwrites previous data. Use with care.
%%
%% @param none
%%
%% @returns term()
%%
%% @end
-spec create_cluster_metadata() -> term().
create_cluster_metadata() ->
   {ok,L} = application:get_env(system_metadata, cluster),
   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, L),
   try                                                                                                                                       
      erpc:multicall(Nodes, sbsystem, create_cluster_metadata, [], 1000)                                                                                 
   catch                                                                                                                                     
      error:{erpc,noconnection} -> io:format("Node is not reachable~n", [])                                                                  
   end.     

% Interface 1

%% @doc Creates a new Tuple Space named as TS 
%%
%% @param TS = string()
%%
%% @returns term()
%%
%% @end
-spec new(string()) -> term().
new(TS) ->
   try
     {_, Nodes} = sbts:nodes(TS),
     % New TS only if it doesn't exist
     % otherwise use method AddNode
     case Nodes of
        [tuple_space_does_not_exist] -> sb:command({new, TS, node()});
                                   _ -> {err, ts_name_already_exists}
     end
   catch Error:Reason ->
      case Reason of
         {ts_name_already_exists} -> {err, Reason};
                                _ -> {Error, Reason}
      end
   end.

%% @doc Returns a tuple matching the pattern in the TS and deletes it from the TS.
%% Blocks if there is no tuple matching 
%%
%% @param TS = string(), Pattern = tuple() 
%%
%% @returns term()
%%
%% @end
-spec in(string(), tuple()) -> term().
in(TS, Pattern) ->
   match_pattern(TS, Pattern, in).	

%% @doc Returns a tuple matching the pattern in the TS
%% Blocks if there is no tuple matching
%%
%% @param TS = string(), Pattern = tuple() 
%%
%% @returns term()
%%
%% @end
-spec rd(string(), tuple()) -> term().
rd(TS, Pattern) ->
  match_pattern(TS, Pattern, rd).

%% @doc Match the pattern Pattern through Function (rd|in) on the tuple space TS
%%
%% @param TS = string(), Pattern = tuple(), Function = atom() = rd | in
%%
%% @returns
%%
%% @end
-spec match_pattern(string(), tuple(), atom()) -> term().
match_pattern(TS, Pattern, Function) ->
   {_, {Ret, Match}} =  sb:command({Function, TS, Pattern}),
   case Ret of
     no_match -> sbts:halt();
            _ -> Match
   end.

%% @doc Puts the tuple Tuple on the Tuple Space TS
%%
%% @param TS = string(), Tuple = tuple()
%%
%% @returns
%%
%% @end
-spec out(string(), tuple()) -> term().
out(TS, Tuple) ->
   sb:command({out, TS, Tuple}).

% Interface 2

%% @doc The same as the rd above, but with a timeout
%%
%% @param TS = string(), Pattern = tuple(), Timeout = integer()
%%
%% @returns term()
%%
%% @end
-spec rd(string(), tuple(), integer()) -> term().
rd(TS, Pattern, Timeout) ->
   match_pattern_timeout(TS, Pattern, Timeout, rd).

%% @doc The same as the in above, but with a timeout
%%
%% @param TS = string(), Pattern = tuple(), Timeout = integer()
%%
%% @returns term()
%%
%% @end
-spec in(string(), tuple(), integer()) -> term().
in(TS, Pattern, Timeout) ->
   match_pattern_timeout(TS, Pattern, Timeout, in).

%% @doc The same as the match_pattern above but with a timeout
%%
%% @param TS = string(), Pattern = tuple(), Timeout = integer(), Function = atom() = rd | in
%%
%% @returns
%%
%% @end
-spec match_pattern_timeout(string(), tuple(), integer(), atom()) -> term().	
match_pattern_timeout(TS, Pattern, Timeout, Function) ->
   try	
      {_, {Ret, Match}} =  sb:command({Function, TS, Pattern}),
      case Ret of
        no_match -> sbts:halt(Timeout);
               _ -> Match
      end
   catch Error:Reason ->
      case Reason of 
         {timeout, _} -> {err, timeout};
	           _  -> {Error, Reason}
      end
   end.

% Interface 3

%% @doc Adds the Node to the TS
%%
%% @param TS = string(), Node = atom()
%%
%% @returns term()
%%
%% @end
-spec addNode(string(), atom()) -> term().
addNode(TS, Node) ->
   sb:command({addNode, TS, Node}).

%% @doc Removes the Node from the TS
%%
%% @param TS = string(), Node = atom()
%%
%% @returns term()
%%
%% @end
-spec removeNode(string(), atom()) -> term().
removeNode(TS, Node) ->
   sb:command({removeNode, TS, Node}).

%% @doc Tells the nodes on which the TS is visible/replicated
%%
%% @param TS = string()
%%
%% @returns term()
%%
%% @end
-spec nodes(string()) -> term().
nodes(TS) ->
   sb:command({nodes, TS}).

