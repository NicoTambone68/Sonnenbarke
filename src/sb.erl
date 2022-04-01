%% See also:
%%% https://github.com/rabbitmq/ra-examples/blob/master/refcell/src/refcell.erl

-module(sb).
-behaviour(ra_machine).
-include("sys_meta.hrl").

-export([
         %% ra_machine implementation
         init/1, init/0,
         apply/3,
	 state_enter/2,
	 system_effects/1,
	 command_effects/1,

         %% Client api
	 system_command/1,
	 command/1,
	 broadcast_command/5,
	 restart/0,

         %% Cluster management API
	 create_cluster_metadata/0,
	 get_cluster_metadata/1,
	 update_cluster_metadata/1,
         start_cluster/0,
	 stop_cluster/0
        ]).

% ra data files directory
-define(RA_HOME_DIR, './.ra').
% minimum required nodes to form a cluster
-define(MIN_NODES, 3).
% System Metadata Table Name
-define(SYSTEM, #sys_meta).

%% ra_machine implementation

init(_Config) -> 
   % sbsystem is a gen_server which handles system's metadata
   sbsystem:start(),
   % sbts is a gen_server which handles Tuple Spaces
   sbts:start(),
   [ok]. %#{}.


init() ->
   application:ensure_all_started(ra),
   init(init).


apply(_Meta, Param, State) ->
	case Param of
	   %% System messages 
           {system, Value} ->
             case Value of
                {set_cluster_state, ClusterState}
		 when ClusterState =:= open; ClusterState =:= closed ->
                   NewState = ClusterState,
	           Reply = ok,
	           Effects = [{mod_call, ?MODULE, system_effects, [Value]}];
                _ ->
                   NewState = State,
	           Reply = {error, invalid_system_message},
	           Effects = []	   
             end,
	     {NewState, Reply, Effects};
	   {command, Value} ->
              io:format("Received command ~p~n", [Value]),
	      % io:format("Meta ~p~n", [_Meta]),
	      Effects = [{mod_call, ?MODULE, command_effects, [Value]}],
              {State, ok, Effects}
	end.

system_effects(Value) ->
   case Value of
      {set_cluster_state, ClusterState} 
	when ClusterState =:= open; ClusterState =:= closed ->
         {_, ClusterMetaData} = sbsystem:get_cluster_metadata(),
         NewClusterMetaData = ClusterMetaData?SYSTEM{status = ClusterState},
	 update_cluster_metadata(NewClusterMetaData),
	 update_followers_metadata(NewClusterMetaData),
         io:format("Cluster state: ~p~n", [ClusterState]);
      _ -> {error, invalid_system_effect_parameter}
   end.	   

% TO DO: get_scn() only for transactions since it's useless for read-only operations
command_effects(Value) ->
   io:format("Received command: ~p~n", [Value]),
   case Value of
      {new, Name} -> 
		     sbts:new(Name),
		     % TO DO: check if Scn is correctly applied
                     Scn = sbsystem:get_scn(),
		     sbsystem:update_cluster_metadata(Scn),
                     {_, ClusterMetaData} = sbsystem:get_cluster_metadata(),
                     update_followers_metadata(ClusterMetaData),
                     [{TSName, _} | _ ] = ClusterMetaData?SYSTEM.ts,
                     % Now create the Tuple Spaces on the Leader Node
                     sbdbs:open_table(TSName),
	             % Broadcast the wake up message to all the cluster's nodes
	             Nodes = ClusterMetaData?SYSTEM.nodes,
                     [{sbts, N}!new_tuple_in || N <- Nodes];

      {out, TS, Tuple} ->	   
		     sbts:out(TS, Tuple),
                     Scn = sbsystem:get_scn(),
		     sbsystem:update_cluster_metadata(Scn),
                     {_, ClusterMetaData} = sbsystem:get_cluster_metadata(),
                     update_followers_metadata(ClusterMetaData);

      {rd, TS, Pattern} ->
		     sbts:rd(TS, Pattern);
		     % No need to update metadata here bcs nothing has changed
		     %
      {in, TS, Pattern} ->
		     %sbts:in(TS, Pattern)
		     % No need to update metadata here bcs Pattern was deleted nothing has changed
		     {Result, Nodes} = sbts:nodes(TS),
		     case Result of
		        ok -> ?MODULE:broadcast_command(sbts, in, [TS, Pattern], infinity, Nodes);
			 _ -> ts_doesnt_exist
		     end;

      {addNode, TS, Node} ->
		     {Result, _} = sbts:nodes(TS),
		     case Result of
		        ok -> sbts:addNode(TS, Node);
			 _ -> ts_doesnt_exist
		     end,
		     % TO DO: replicate TS datafile to the new node
		     %        thus copy dets file or recreate the table?
		     % TO DO: put the following code in update_all_metadata/0
                     Scn = sbsystem:get_scn(),
		     sbsystem:update_cluster_metadata(Scn),
                     {_, ClusterMetaData} = sbsystem:get_cluster_metadata(),
                     update_followers_metadata(ClusterMetaData);

      {removeNode, TS, Node} ->
		     {Result, _} = sbts:nodes(TS),
		     case Result of
		        ok -> sbts:removeNode(TS, Node);
			 _ -> ts_doesnt_exist
		     end,
		     % TO DO: delete the dets datafile from the node
		     % TO DO: put the following code in update_all_metadata/0
                     Scn = sbsystem:get_scn(),
		     sbsystem:update_cluster_metadata(Scn),
                     {_, ClusterMetaData} = sbsystem:get_cluster_metadata(),
                     update_followers_metadata(ClusterMetaData);

	   {nodes, TS} ->
		   sbts:nodes(TS);

		     _ -> io:format("Invalid command")
   end.


% Execute Module:Function on the given nodes throgh erpc call
broadcast_command(Module, Function, Args, Timeout, Nodes) ->
 io:format("Broadcasting command: ~p~n", [Function]), 
  try 
     erpc:multicall(Nodes, Module, Function, Args, Timeout)
  catch
     error:{erpc,noconnection} -> io:format("Node is not reachable~n", [])
  end.


% TO DO: add parameter Metadata and implement the function
%        use erpc:multicall instead of erpc:call
update_followers_metadata(ClusterMetaData) -> 
   % Update metadata on every node of the cluster but the leader node which is already updated 
   % This function must be called solely by the leader through one of the Effects method
   Nodes = ClusterMetaData?SYSTEM.nodes,
   [io:format("Updating Metadata on node ~p, Result: ~p~n",  
   	      [N, 
	        try erpc:call(N, ?MODULE, update_cluster_metadata, [ClusterMetaData]) of
		   _ -> ok
	        catch
		   error:{erpc,noconnection} -> io:format("Node is not reachable~n", [])
		end
	      ]) || N <- Nodes, N /= node()],
   ok.


state_enter(leader, _) ->
    ServerId = {ra_cluster, node()},
    io:format("Current leader node is ~p~n", [ServerId]),
    [];
state_enter(_, _) ->
    [].

%% Client api


system_command(Value) ->
    ClusterName = sbsystem:get_cluster_name(),
    {ok, Result, _Leader} = ra:process_command({ClusterName, node()}, {system, Value}),
    Result.


command(Command) ->
    ClusterName = sbsystem:get_cluster_name(),
    {ok, Result, _Leader} = ra:process_command({ClusterName, node()}, {command, Command}),
    Result.


% restart after a crash
restart() ->
    % open tables in order to force repair 
    % then close them again
    % TO DO: create a function on the sbsystem module
    sbdbs:open_tables(),
    sbdbs:close_tables(),
    {_, CMeta} = ?MODULE:get_cluster_metadata(disk),
    ra:start_in(?RA_HOME_DIR),
    ra:restart_server({CMeta?SYSTEM.name, node()}),
    ?MODULE:init().


%% Cluster api
%%
%%
%% From == ram | disk
get_cluster_metadata(From) ->
   sbdbs:open_tables(),
   {Response, ClusterMetaData} = sbdbs:get_cluster_metadata(From),
   {Response, ClusterMetaData}.


update_cluster_metadata(ClusterMetaData) ->
   sbdbs:update_cluster_metadata(ClusterMetaData, both).


% you need at least three nodes to form a cluster 
check_nodes_list(Nodes) when length(Nodes) >= ?MIN_NODES -> ok;
check_nodes_list(Nodes) when length(Nodes) <  ?MIN_NODES -> not_enough_nodes.

check_active_nodes(Responses) ->
   Count = lists:foldl(fun(R, X) -> if R == pong -> X+1; R == pang -> X end end, 0, Responses),
   if Count >= ?MIN_NODES -> ok;
      Count  < ?MIN_NODES -> not_enough_active_nodes
   end.		   


% TO DO: no! prendere i metadati dallo storage locale.
% fare controllo integrita', partire solo se ok
% e start di sbsystem solo al termine
start_cluster() ->
%   {ok, Pid} = sbsystem:start(),
%   case is_process_alive(Pid) of
   sbdbs:open_tables(),
   {Response, Cluster} = sbdbs:get_cluster_metadata(disk),
   sbdbs:close_tables(),
   case Response of
      ok ->
         Nodes = Cluster?SYSTEM.nodes,	
         % Nodes = ['ra1@localhost', 'ra2@localhost', 'ra3@localhost', 'ra4@localhost'],
         case check_nodes_list(Nodes) of
            ok ->
               [io:format("Attempting to communicate with node ~s, response: ~s~n", [N, net_adm:ping(N)]) || N <- Nodes],
               Ping=[net_adm:ping(N) || N <- Nodes],
               case check_active_nodes(Ping) of
                  not_enough_active_nodes -> 
	             handle_starting_failure("You need at least three active nodes to form a cluster");     
                  ok ->
                     % see erpc:call for possible improvement
                     [io:format("Initializing node ~s, response: ~p~n",
		        [N, 
		           try erpc:call(N, ?MODULE, init, []) of
		              _ -> ok
			   catch
		              _ -> io:format("Initialization of node ~s has failed~n", [N])
			   end
		        ]) || N <- Nodes],
                     [io:format("Starting Ra on node ~s, response: ~p~n", 
                        [N,
			   try erpc:call(N, ra, start_in, [?RA_HOME_DIR]) of
		              _ -> ok
		           catch
		              _ -> io:format("Starting Ra on node ~s has failed~n", [N])
			   end
                        ]) || N <- Nodes],
                     Name = Cluster?SYSTEM.name,  %ra_cluster,
                     ServerIds = [{Name, N} || N <- Nodes],
                     MachineConf = {module, ?MODULE, #{}},
		     {Result, Started, NotStarted} = ra:start_cluster(default, Name, MachineConf, ServerIds),
		     case Result of 
		        ok -> 
                           io:format("Cluster started successfully~n"),
			   io:format("Nodes started ~p~n", [Started]),
			   io:format("Nodes not started ~p~n", [NotStarted]),
			   % TO DO: debug sb:start_cluster() => 
			   %              sb:stop_cluster() => sb:start_cluster()
			   %              CRASH (RAM dets not there)
                           ?MODULE:system_command({set_cluster_state, open});
			error ->
                           handle_starting_failure("Cluster failed to start on Ra")
		     end
               end;
            _ -> 
               handle_starting_failure("You need at least three nodes to form a cluster")
         end;
      error -> handle_starting_failure("A problem occurred: sys_meta.dets not found. Error creating default")
   end.		   


handle_starting_failure(Reason) ->
    io:format("Cluster not started. Reason: ~s~n", [Reason]),
    cluster_not_started.


stop_cluster() ->
   ?MODULE:system_command({set_cluster_state, closed}),
   stopping_sequence(),
   cluster_stopped.


stopping_sequence() ->
   {_, Cluster} = get_cluster_metadata(ram),
   case Cluster of
      no_data -> 
         {error, error_retrieving_metadata};
      _ ->
         Nodes = Cluster?SYSTEM.nodes,
         [io:format("Stopping Ra on node ~s, response: ~p~n", 
                 [N, rpc:call(N, ra, stop_server, [default, {Cluster?SYSTEM.name, N}])]) || N <- Nodes],
         [rpc:call(N, sbsystem, stop, []) || N <- Nodes],
         [rpc:call(N, sbts, stop, []) || N <- Nodes]
   end.

% Create a new metadata storage 
%  WARNING! Overwrites data
create_cluster_metadata() ->
   sbsystem:create_cluster_metadata().
   

