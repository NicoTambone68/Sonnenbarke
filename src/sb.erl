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
	 my_effects/2,
	 system_effects/1,
	 command_effects/1,
	 

         %% Client api
         send_msg/2,
	 send_msg2/1,
	 system_msg/1,
	 command/1,
	 restart/0,
%	 restart/1,

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
	sbsystem:start(),
	[ok]. %#{}.


init() ->
    application:ensure_all_started(ra),
    init(init).

apply(_Meta, Param, State) ->
	case Param of
           {system, From, Value, Scn} ->
             io:format("~p Dest: ~p Received system message ~p with state ~p and SCN ~p~n", 
     			 [maps:get(system_time, _Meta), From, Value, State, Scn]),
             NewState = open,
	     Reply = ok,
	     Effects = [{mod_call, ?MODULE, system_effects, [Scn]}],
	     {NewState, Reply, Effects};
           {send_msg, Value, Scn} ->
             {[io:format("Received message #1 ~p with state ~p and SCN ~p~n", [Value, State, Scn])], ok};
           {send_msg2, Value, Scn} ->
             io:format("Received message #2 ~p with state ~p and SCN ~p~n", [Value, State, Scn]),
	     Effects = [{mod_call, ?MODULE, my_effects, [State, Scn]}],
             {State, ok, Effects};
	   {command, Value} ->
              io:format("Received command ~p~n", [Value]),
	      Effects = [{mod_call, ?MODULE, command_effects, [Value]}],
              {State, ok, Effects}
	end.
		

my_effects(State, Scn) ->
   io:format("Effect triggered by system message. Leader: ~p, state: ~p, SCN:~p~n", [node(), State, Scn]).

system_effects(Scn) ->
    io:format(" Updating followers SCN to ~p~n", [Scn]).

command_effects(Value) ->
   Scn = sbsystem:get_scn(),
   io:format("Received command: ~p. Now updating followers SCN to ~p~n", [Value, Scn]),
   {_, ClusterMetaData} = sbsystem:get_cluster_metadata(),
   Nodes = ClusterMetaData#sys_meta.nodes,
   % Update metadata on every node of the cluster but the leader node which is already updated 
   [io:format("Updating Metadata on node ~p, Result: ~p~n",  
   	      [N, 
	        try erpc:call(N, ?MODULE, update_cluster_metadata, [ClusterMetaData]) of
		   _ -> ok
	        catch
		   error:{erpc,noconnection} -> io:format("Node is not reachable~n", [])
		end
	      ]) || N <- Nodes, N /= node()].
   

state_enter(leader, _) ->
    ServerId = {ra_cluster, node()},
    io:format("Current leader node is ~p~n", [ServerId]),
    [];
state_enter(_, _) ->
    [].

%% Client api

%% Test Nick
send_msg(ServerId, Value) ->
    {ok, Result, _Leader} = ra:process_command(ServerId, {send_msg, Value}),
     io:format("Current leader is ~p~n", [_Leader]),
    Result.

send_msg2(Value) ->
    % TO DO: switch to persistent term?
    % {_, CMeta} = sbdbs:get_cluster_metadata(ram),
    ClusterName = sbsystem:get_cluster_name(),
%    {ok, Result, _Leader} = ra:process_command({ClusterName, node()}, {send_msg2, Value, sbsystem:get_scn()}),
    {ok, Result, _Leader} = ra:process_command({ClusterName, node()}, {send_msg2, Value, 999}),
     io:format("Current leader is ~p~n", [_Leader]),
    Result.

system_msg(Value) ->
    ClusterName = sbsystem:get_cluster_name(),
    Scn = 9991,
    {ok, Result, _Leader} = ra:process_command({ClusterName, node()}, {system, node(), Value, Scn}),
    Result.


command(Command) ->
    ClusterName = sbsystem:get_cluster_name(),
    {ok, Result, _Leader} = ra:process_command({ClusterName, node()}, {command, Command}),
    Result.



% restart after a crash
%restart(Node) ->
%    Name = ra_cluster,
%    % ra:start(),
%    % § TO DO: Test this function
%    % data_dir, file:filename()}
%    rpc:call(Node, ra, start_in, [?RA_HOME_DIR]),
%    ra:restart_server({Name, Node}).

restart() ->
    % open tables in order to force repair 
    % then close them again
    % TO DO: create a function on the sbsystem module
    sbdbs:open_tables(),
    sbdbs:close_tables(),
    {_, CMeta} = ?MODULE:get_cluster_metadata(disk),
%%%%    restart(node()),
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
         Nodes = Cluster#sys_meta.nodes,	
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
                     [io:format("Initializing node ~s, response: ~p~n", [N, rpc:call(N, ?MODULE, init, [])]) || N <- Nodes],
                     [io:format("Starting Ra on node ~s, response: ~p~n", [N, rpc:call(N, ra, start_in, [?RA_HOME_DIR])]) || N <- Nodes],
                     Name = Cluster#sys_meta.name,  %ra_cluster,
                     ServerIds = [{Name, N} || N <- Nodes],
                     MachineConf = {module, ?MODULE, #{}},
		     {Result, Started, NotStarted} = ra:start_cluster(default, Name, MachineConf, ServerIds),
		     case Result of 
		        ok -> 
                           io:format("Cluster started successfully~n"),
			   io:format("Nodes started ~p~n", [Started]),
			   io:format("Nodes not started ~p~n", [NotStarted]);
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
   stopping_sequence(),
   cluster_stopped.

%to do: check why actually don't stop the server
stopping_sequence() ->
   {_, Cluster} = get_cluster_metadata(ram),
   case Cluster of
      no_data -> 
         {error, error_retrieving_metadata};
      _ ->
         Nodes = Cluster#sys_meta.nodes,
         [io:format("Stopping Ra on node ~s, response: ~p~n", 
                 [N, rpc:call(N, ra, stop_server, [default, {Cluster#sys_meta.name, N}])]) || N <- Nodes],
         [rpc:call(N, sbdbs, close_tables, []) || N <- Nodes]
   end.

% Create a new metadata storage 
%  WARNING! Overwrites data
create_cluster_metadata() ->
   sbsystem:create_cluster_metadata().
   

