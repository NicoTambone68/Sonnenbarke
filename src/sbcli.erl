-module(sbcli).

-export([
	 % Client functions
	 connect/0,
	 start/0,
	 stop/0,
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
connect() -> {ok}.

start() ->
   sb:start_cluster().

stop() ->
   sb:stop_cluster().	

metadata() ->
   sb:get_cluster_metadata(ram).

set_leader(Node) ->
   ClusterName = sbsystem:get_cluster_name(),
   {_, Leader} = ra_leaderboard:lookup_leader(ClusterName),
   ra:transfer_leadership({ClusterName, Leader}, {ClusterName, Node}).

% initialize new metadata on all nodes
% based on sys.config
% create directories if not exist 
% 
create_cluster_metadata() ->
   {ok,L} = application:get_env(system_metadata, cluster),
   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, L),
   try                                                                                                                                       
      erpc:multicall(Nodes, sbsystem, create_cluster_metadata, [], 1000)                                                                                 
   catch                                                                                                                                     
      error:{erpc,noconnection} -> io:format("Node is not reachable~n", [])                                                                  
   end.     

% Interface 1

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

in(TS, Pattern) ->
   match_pattern(TS, Pattern, in).	

rd(TS, Pattern) ->
  match_pattern(TS, Pattern, rd).

% Function atom() rd | in
match_pattern(TS, Pattern, Function) ->
   {_, {Ret, Match}} =  sb:command({Function, TS, Pattern}),
   case Ret of
     no_match -> sbts:halt();
            _ -> Match
   end.


out(TS, Tuple) ->
   sb:command({out, TS, Tuple}).

% Interface 2

rd(TS, Pattern, Timeout) ->
   match_pattern_timeout(TS, Pattern, Timeout, rd).

in(TS, Pattern, Timeout) ->
   match_pattern_timeout(TS, Pattern, Timeout, in).

%	
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

addNode(TS, Node) ->
   sb:command({addNode, TS, Node}).

removeNode(TS, Node) ->
   sb:command({removeNode, TS, Node}).

nodes(TS) ->
   sb:command({nodes, TS}).

