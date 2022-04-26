%%
%%
%%
%% @doc The main module for starting, stopping and interacting with the Ra cluster

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
	 restart/0,

         %% Cluster management API
	 create_cluster_metadata/0,
	 get_cluster_metadata/1,
	 update_cluster_metadata/1,
	 update_all_metadata/0,
         start_cluster/0,
	 stop_cluster/0
        ]).

% minimum required nodes to form a cluster
-define(MIN_NODES, 3).

% System Metadata Table Name
-define(SYSTEM, #sys_meta).

%% ra_machine implementation

-spec init(term()) -> [ok].
init(_Config) -> 
   % sbsystem is a gen_server which handles system's metadata
   sbsystem:start(),
   % sbts is a gen_server which handles Tuple Spaces
   sbts:start(),
   [ok]. %#{}.

-spec init() -> term().
init() ->
   application:ensure_all_started(ra),
   init(init).

%% @doc ra_machine callback function
%% This function allows to process messages sent to the cluster.
%% This is done by means of the Reply fucntion which is executed by each node
%% The optional Effects function is executed only by the Leader
%%
%% @param 
%% _Meta: ra_machine metadata
%% Param: message sent by the caller
%%        {system, Value} | {command, Value}
%% State: ra_machine state
%%
%% @returns
%% {State, Reply} | {State, Reply, Effects}
%%
%% @end
-spec apply(term(), term(), term()) -> term().
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
              % command_effects should be called through mod_call through Effects
	      % but we need to send back Reply, so non blocking effects are called here
	      Reply = ?MODULE:command_effects(Value),
	      % Any command wich modifies data must update metadata
	      % This is done by the Leader through Effects
	      Command = lists:nth(1, tuple_to_list(Value)),
	      % CommandEffects = {mod_call, ?MODULE, command_effects, [Value]},
	      case lists:member(Command, [new, in, out, 'addNode', 'removeNode']) of
		      true  -> Effects = [{mod_call, ?MODULE, update_all_metadata, []}],
			       {State, Reply, Effects};

		      false -> {State, Reply} 
	      end
	end.

%% @doc Reply function for internal System's messages
%% Sset the cluster state to the new state (open | close).
%% Update Leader metadata, then update all the followers metadata.
%%
%% @param 
%% Value: {system, {set_cluster_state, ClusterState}}
%%
%% @returns
%% when a wrong parameter was passed: {error, invalid_system_effect_parameter}
%%
%% @end
%%
-spec system_effects(term()) -> term().
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

%% @doc Updates Leader and followers metadata after a transaction 
%% Every transaction il labeled with a unique Scn (System Change Number)
%% The currenta value of Scn is recorded into the cluster's metadata
%%
%% @param 
%% none
%%
%% @returns
%% none
%%
%% @end
% system utility: updates Leader and followers metadata after a transaction
-spec update_all_metadata() -> term(). 
update_all_metadata() ->
   Scn = sbsystem:get_scn(),
   {_, ClusterMetaData} = sbsystem:update_cluster_metadata(Scn),
   update_followers_metadata(ClusterMetaData).

%% @doc Command interpreter 
%%
%% @param Value =  {new, Name, Node} | {out, TS, Tuple} | {rd, TS, Pattern} 
%% | {in, TS, Pattern} | {addNode, TS, Node} | {removeNode, TS, node} | {nodes, TS}
%%
%% @returns
%% Result of the operation | {invalid_command}
%%
%% @end
%%
-spec command_effects(term()) -> term().
command_effects(Value) ->
   io:format("Received command: ~p~n", [Value]),
   case Value of
      {new, Name, Node} -> 
		     % Create a new TS Name on Node
                     {_, Nodes} = sbts:nodes(Name),
		     case Nodes of
			% new TS only if it doesn't already exist on Node     
			% first case is a brand new ts
                        [tuple_space_does_not_exist] -> Action = new_name_node;
                        % second case: ts already exists. we're going to check if it's on Node or not
                                                   _ -> case lists:member(Node, Nodes) of
								true -> Action = nothing_to_do;
						               false -> Action = new_name_node
							end
                      end,            
                      % Execute only if node() == Node and the tests above say go
		      case (Action =:= new_name_node) and (node() =:= Node)  of
                              true -> Return = sbts:new(Name, Node);

                                 _ -> Return =  ok
                      end;

			     

      {out, TS, Tuple} ->	 
         Return = interface_1(out, TS, Tuple);


      {rd, TS, Pattern} ->
         Return = interface_1(rd, TS, Pattern);


      {in, TS, Pattern} ->
         Return = interface_1(in, TS, Pattern);


      {addNode, TS, Node} ->
%         sbts:addNode(TS, Node),
%         Return = {{addNode, TS, Node}, ok};

		     {_, Nodes} = sbts:nodes(TS),
		     case Nodes of
		        [tuple_space_does_not_exist] ->
                              sbts:new(TS, Node),    %sbts:addNode(TS, Node);
                              % NodeFrom = lists:nth(1, Nodes),
                              % copy_ts(TS, NodeFrom, Node),
                              Return = {{addNode, TS, Node}, ok};
                        
			 % TS already on cluster. Add Node if not a member of the TS
			 _ -> case lists:member(Node, Nodes) of
                                 true -> Return = {{addNode, TS, Node}, ok};

				false -> sbts:new(TS, Node),
					 % Note that the new node is added to the Head of the list
					 % so take care to pick the last element to copy from
                                         NodeFrom = lists:last(Nodes),
                                         sbts:copy_ts(TS, NodeFrom, Node),
                                         Return = {{addNode, TS, Node}, ok}
			      end 
		     end;


      {removeNode, TS, Node} ->
		     % {Result, Nodes} = sbts:nodes(TS),
		     {Result, _} = sbts:nodes(TS),
		     case Result of
		        ok -> sbts:removeNode(TS, Node),
			      % remove physical datafile 
			      % delete it only in the given Node
			      case Node =:= node() of
                                 true -> sbdbs:delete_table(TS),
		                         % What if we remove the only associated node? Remove TS as well
					 {_, Nodes} = sbts:nodes(TS),
                                         case length(Nodes) of
                                            1 -> sbts:removeTS(TS);
                                            _ -> ok
		                          end;

                                false -> nothing_to_do
			      end;

			 _ -> ts_doesnt_exist
		     end,
		     Return = {{removeNode, TS, Node}, ok};


	   {nodes, TS} ->
		   Return = sbts:nodes(TS);


		     _ -> Return = {invalid_command}
     end,
     Return.

%% @doc Abstract implementation of all the functions described as the "Interface 1/3"
%% of the specifications. 
%%
%% @param Function: new | in | rd | out; TS = Tuple Space name; 
%% Tuple is whatever tuple to be processed by Function.
%% When Function = new, also sends the message new_tuple_in
%% to the module sbts of all of the cluster nodes.
%% This is the wake-up event for blocking operations in and rd.
%% (see project specifications for details)
%%
%% @returns
%% An array of tuple [{tuple()}] or other results according to the function
%% or {err, ts_doesnt_exist} if the tuple space TS doesn't exist
%% @end
%%
-spec interface_1(string(), string(), tuple()) -> term().
interface_1(Function, TS, Tuple) ->
   % Get the list of nodes associated with TS 
   {Result, Nodes} = sbts:nodes(TS),
   case Result of
      ok ->  % Execute only on associated nodes
             case lists:member(node(), Nodes) of
                true -> Return = sbts:Function(TS, Tuple),
			% out must send wake up signal new_tuple_in
			case Function of
				out -> {_, ClusterMetaData} = sbsystem:get_cluster_metadata(),
				       ClusterNodes = ClusterMetaData?SYSTEM.nodes,
                                       [{sbts, N}!new_tuple_in || N <- ClusterNodes];

                                  _ -> ok
			end;

               false -> Return = {ok, no_action}
             end;

      _ -> Return = {err, ts_doesnt_exist}
   end,
   Return.

%% @doc Updates metadata on every node of the cluster but the leader node which is already updated.
%% This function must be called solely by the leader through one of the Effects method.
%%
%% @param ClusterMetadata: record containing all the cluster's metadata. See sys_meta.hrl
%%
%% @returns ok
%%
%% @end
%%
-spec update_followers_metadata(term()) -> term().
update_followers_metadata(ClusterMetaData) -> 
   Nodes = ClusterMetaData?SYSTEM.nodes,
   erpc:multicall(Nodes, ?MODULE, update_cluster_metadata, [ClusterMetaData]),
   ok.

%% @doc Callback function of the ra_machine. It's triggered whenever the cluster's state changes
%% Here we implement the callback to signal when a node becomes leader 
%%
%% @param leader:atom()
%%
%% @returns
%% none
%%
%% @end
-spec state_enter(term(), term()) -> term().
state_enter(leader, _) ->
    io:format("Current leader node is ~p~n", [node()]),
    [];
state_enter(_, _) ->
    [].

%% Client api

%% @doc Sends a system command to the cluster
%%
%% @param Value (see system_effects)
%%
%% @returns Result
%%
%% @end
%%
-spec system_command(term()) -> term().
system_command(Value) ->
    ClusterName = sbsystem:get_cluster_name(),
    {ok, Result, _Leader} = ra:process_command({ClusterName, node()}, {system, Value}),
    Result.

%% @doc Sends a TS command to the cluster
%%
%% @param Command (see command_effects)
%%
%% @returns {Result, Reply}
%%
%% @end
%%
-spec command(term()) -> term().
command(Command) ->
    ClusterName = sbsystem:get_cluster_name(),
    {Result, Reply, _Leader} = ra:process_command({ClusterName, node()}, {command, Command}, 20000),
    {Result, Reply}.


%% @doc Restart a node previously started and then crashed  
%%
%% @param none
%%
%% @returns none
%%
%% @end
%%
-spec restart() -> term().
restart() ->
    % open tables in order to force repair 
    % then close them again
    sbdbs:open_tables(),
    sbdbs:close_tables(),
    {ok, RaHomeDir} = sbenv:get_cluster_env(ra_home_dir), 
    {_, CMeta} = ?MODULE:get_cluster_metadata(disk),
    ra:start_in(RaHomeDir),
    ra:restart_server({CMeta?SYSTEM.name, node()}),
    ?MODULE:init().


%% @doc Utility function to visualize the current cluster's metadata
%% (See sys_meta.hrl form metadata's structure)
%%
%% @param From: ram|disk
%%
%% @returns {Response, ClusterMetdata)
%%
%% @end
%%
-spec get_cluster_metadata(term()) -> term().
get_cluster_metadata(From) ->
   sbdbs:open_tables(),
   {Response, ClusterMetaData} = sbdbs:get_cluster_metadata(From),
   {Response, ClusterMetaData}.


%% @doc Update cluster's metadata with current data on both ram and disk
%%
%% @param ClusterMetadata (see sys_meta.hrl)
%%
%% @returns {ok, ClusterMetaData}
%%
%% @end
%%
-spec update_cluster_metadata(term()) -> term().
update_cluster_metadata(ClusterMetaData) ->
   sbdbs:update_cluster_metadata(ClusterMetaData, both).


%% @doc Checks the list of the cluster nodes to contain at least the minumum number 
%% of nodes needed to form a cluster (usually 3).
%%
%% @param Nodes:List().
%%
%% @returns ok | not_enaugh_nodes
%%
%% @end
%%
-spec check_nodes_list(term()) -> term().
check_nodes_list(Nodes) when length(Nodes) >= ?MIN_NODES -> ok;
check_nodes_list(Nodes) when length(Nodes) <  ?MIN_NODES -> not_enough_nodes.

%% @doc Check every node contained in the given list, by pinging it.
%% Returns ok if the number of responsive nodes is enaugh to form a cluster
%% 
%% @param Nodes:List()
%%
%% @returns ok | not_enough_active_nodes
%%
%% @end
%%
-spec check_active_nodes(term()) -> term().
check_active_nodes(Nodes) -> 
   Count = lists:foldl(fun(R, X) -> if R == pong -> X+1; R == pang -> X end end, 0, Nodes),
   if Count >= ?MIN_NODES -> ok;
      Count  < ?MIN_NODES -> not_enough_active_nodes
   end.		   

%% @doc Initializes and starts the cluster.
%%
%% @param none
%%
%% @returns ok | cluster_not_started
%%
%% @end
%%
-spec start_cluster() -> term().
start_cluster() ->
   io:format("Sonnenbarke. A tuple space management system with Ra~n", []),
   io:format("Nicolò Tambone - UniUrb ADCC~n", []),
   {ok, RaHomeDir} = sbenv:get_cluster_env(ra_home_dir), 
   sbdbs:open_tables(),
   {Response, Cluster} = sbdbs:get_cluster_metadata(disk),
   sbdbs:close_tables(),
   TimeOut = 200,
   case Response of
      ok ->
         Nodes = Cluster?SYSTEM.nodes,	
         case check_nodes_list(Nodes) of
            ok ->
               [io:format("Connecting to node ~s: ~s~n", [N, ping_node(N)]) || N <- Nodes],
               Ping=[net_adm:ping(N) || N <- Nodes],
               case check_active_nodes(Ping) of
                  not_enough_active_nodes -> 
	             handle_starting_failure("You need at least three active nodes to form a cluster");     
                  ok ->
                     io:format("Initializing nodes ~n",[]),
		     erpc:multicall(Nodes, ?MODULE, init, [], TimeOut),

                     timer:sleep(2000),
                     
                     io:format("Starting Ra ~n", []),
		     erpc:multicall(Nodes, ra, start_in, [RaHomeDir], TimeOut), 

                     timer:sleep(2000),
		     
                     Name = Cluster?SYSTEM.name,  %ra_cluster,
                     ServerIds = [{Name, N} || N <- Nodes],
                     MachineConf = {module, ?MODULE, #{}},
		     Result = ra:start_cluster(default, Name, MachineConf, ServerIds),
		     case Result of 
		        {ok, Started, NotStarted} -> 
			   case NotStarted of
				   [] -> io:format("All nodes have been started correctly~n", []);
				   _  -> io:format("  The following nodes have been started correctly: ~p~n", [Started]),
				         io:format("WARNING: the following nodes have failed to start: ~p~n", [NotStarted])
			   end,
                           io:format("Cluster started~n~n"),
			   %io:format("Nodes not started ~p~n", [NotStarted]),
                           ?MODULE:system_command({set_cluster_state, open});
                        {error, Reason} ->
                           handle_starting_failure(Reason)
		     end
               end;
            _ -> 
               handle_starting_failure("You need at least three nodes to form a cluster")
         end;
      error -> handle_starting_failure("A problem occurred: sys_meta.dets not found. Error creating default")
   end.		   

%% @doc Wrapper of the BIF net_adm:ping(Node) 
%% just to have the return values of ok and ko instead of pong and pang 
%%
%% @param Node
%%
%% @returns ok|ko
%%
%% @end
%%
-spec ping_node(term()) -> term().
ping_node(Node) ->
   Resp = net_adm:ping(Node),
   case Resp of
      pong -> ok;
	_  -> ko
   end.

%% @doc Signals the reasons of the cluster's starting failure 
%%
%% @param Reasons
%%
%% @returns cluster_not_started
%%
%% @end
%%
-spec handle_starting_failure(string()) -> term().
handle_starting_failure(Reason) ->
    io:format("Cluster not started. Reason: ~s~n", [Reason]),
    cluster_not_started.

%% @doc Stop the cluster previously started
%%
%% @param none
%%
%% @returns cluster_stopped
%%
%% @end
%%
-spec stop_cluster() -> term().
stop_cluster() ->
   ?MODULE:system_command({set_cluster_state, closed}),
   stopping_sequence(),
   cluster_stopped.

%% @doc Takes down all of the cluster processes following the correct sequence
%%
%% @param none 
%%
%% @returns none
%%
%% @end
%%
-spec stopping_sequence() -> term().
stopping_sequence() ->
   TimeOut = 200,
   {_, Cluster} = get_cluster_metadata(ram),
   case Cluster of
      no_data -> 
         {error, error_retrieving_metadata};
      _ ->
         Nodes = Cluster?SYSTEM.nodes,
         io:format("Stopping Cluster~n", []),
         [io:format("Stopping Ra on node ~s, response: ~p~n", 
                 [N, rpc:call(N, ra, stop_server, [default, {Cluster?SYSTEM.name, N}])]) || N <- Nodes],
         erpc:multicall(Nodes, sbsystem, stop, [], TimeOut),
         timer:sleep(2000),
	 erpc:multicall(Nodes, sbts, stop, [], TimeOut),
         io:format("Cluster is down.~n", [])
   end.

%% @doc Creates brand new empty metadata on the current node
%% WARNING: previous metadata will be overwritten 
%%
%% @param none
%%
%% @returns none
%%
%% @end
%%
-spec create_cluster_metadata() -> term().
create_cluster_metadata() ->
   sbsystem:create_cluster_metadata().
   

