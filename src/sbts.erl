-module(sbts).
-behaviour(gen_server).
-include("sys_meta.hrl").

-export([
        % gen_server
	start/0,
	stop/0,
	init/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	halt/0,
	halt/1,
	wake_up/1,

	% interface 1
	new/1,
	in/2,
	rd/2,
	out/2,
	match/1,

	% interface 2
	in/3,
	rd/3,

	% interface 3
	addNode/2,
	removeNode/2,
	nodes/1,

	% utilities not in system requirements
	removeTS/1,
	
	% TEST delete after test
	addNode_/2,
	removeNode_/2,
	removeTS_/1,
	nodes_/1
	]).

-define(SYSTEM, #sys_meta).


%TO DO: implement
%
% TO DO: First time you create a TS the default associated node is 
% the one requesting the new TS. Other nodes can be added later on
% with the given API
new_(Name) -> 
   % register the new TS in the system's metadata table
   % get current metadata
   {_, ClusterMetadata} = sbsystem:get_cluster_metadata(),
   % get current leader node
   % {_, Leader} = ra_leaderboard:lookup_leader(ClusterMetadata?SYSTEM.name),
   % Since new is called exclusively by the Leader, for simplicity we just slap in node() 
   {_, Leader} = {ok, node()},
   % append the new Tuple Space to the list. Note that the new name is
   % associated with the current leader node by default
   TS = [{Name, [Leader]} | ClusterMetadata?SYSTEM.ts],
   % TS = lists:append(ClusterMetadata?SYSTEM.ts, {Name, [Leader]}),
   % Store the updated metadata on a peg variable
   CMetaUpdated = ClusterMetadata?SYSTEM{ts = TS},
   % Save the updated metadata to ram and disk
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   % create dets file
   sbdbs:open_table(Name, create_if_not_exists),
   % close 
   sbdbs:close_table(Name),
   {ok, [Name]}.


in_(TS, Pattern) -> 
   %MatchPattern = ?MODULE:match(Pattern), 
   sbdbs:match_delete_ts(TS, ?MODULE:match(Pattern)).

rd_(TS, Pattern) -> 
   % MatchPattern = ?MODULE:match(Pattern), 
   sbdbs:match_ts(TS, ?MODULE:match(Pattern)).

% Translate the InputPattern with wildcards (any) to a pattern which
% effectively performs dets:match_object 
match(InputPattern) ->
   List = tuple_to_list(InputPattern),
   list_to_tuple(lists:map(fun(X) -> case X of any -> '_'; _ -> X end end, List)).

out_(TS, Tuple) ->
      sbdbs:insert_ts(TS, Tuple).
%   try       	
%      sbdbs:insert_ts(TS, Tuple)
%   catch
%      error:Error -> {error, Error}
%   end.

%   {ok, [Tuple]}.

% Interface 2/3
% Tima out in API Call (see below)

% Interface 3/3
addNode_(TS, Node) -> 
 try
   % get current metadata
   {_, ClusterMetadata} = sbsystem:get_cluster_metadata(),
   % check wheter Node belongs to the cluster or not
   NodeIsRegistered = lists:member(Node, ClusterMetadata?SYSTEM.nodes), 
   if NodeIsRegistered == false -> 
      erlang:error(not_a_cluster_node);
      true -> ok
   end,
   % check wheter Tuple Space TS is already registered into Metadata
   TSisRegistered = lists:member(TS, [TSName || {TSName, _} <- ClusterMetadata?SYSTEM.ts]),
   if TSisRegistered == false -> 
      erlang:error(tuple_space_does_not_exist);
      true -> ok
   end,
   % Take TS data from Metadata
   OldTS = lists:nth(1, [{N,S} || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
   % Take The list of nodes associated with TS
   Nodes = lists:nth(1, [S || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
   % Add the new list of nodes to the TS
   % TS = [{Name, [Leader]} | ClusterMetadata?SYSTEM.ts],
   NewTS = {TS, [Node | Nodes]}, %{TS, lists:append(Nodes, Node)},
   % Remove old TS data from TSList
   TSList = lists:delete(OldTS, ClusterMetadata?SYSTEM.ts),
   % Add the tuple {TS,[Nodes]} to the TSList
   NewTSList = [NewTS | TSList], %lists:append(TSList, NewTS),
   % Store the updated metadata on a peg variable
   CMetaUpdated = ClusterMetadata?SYSTEM{ts = NewTSList},
   % Save the updated metadata to ram and disk
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   % TO DO: replicate TS.dets on the new node
   {ok, [TS, NewTSList]}
 catch
    error:Error -> {error, Error}
 end.	 


removeNode_(TS, Node) -> 
 try
   % get current metadata
   {_, ClusterMetadata} = sbsystem:get_cluster_metadata(),
   % check wheter Node belongs to the cluster or not
   NodeIsRegistered = lists:member(Node, ClusterMetadata?SYSTEM.nodes), 
   if NodeIsRegistered == false -> 
      erlang:error(not_a_cluster_node);
      true -> ok
   end,
   % check wheter Tuple Space TS is already registered into Metadata
   TSisRegistered = lists:member(TS, [TSName || {TSName, _} <- ClusterMetadata?SYSTEM.ts]),
   if TSisRegistered == false -> 
      erlang:error(tuple_space_does_not_exist);
      true -> ok
   end,
   % Take TS data from Metadata
   OldTS = lists:nth(1, [{N,S} || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
   % Take The list of nodes associated with TS
   Nodes = lists:nth(1, [S || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
   % Remove Node from the list of nodes associated with the TS
   NewNodes = [N || N <- Nodes, N /= Node],
   % Add the new list of nodes to the TS
   NewTS = {TS, NewNodes},
   % Remove old TS data from TSList
   TSList = lists:delete(OldTS, ClusterMetadata?SYSTEM.ts),
   % Add the tuple {TS,[Nodes]} to the TSList
   NewTSList = [NewTS | TSList], 
   % Store the updated metadata on a peg variable
   CMetaUpdated = ClusterMetadata?SYSTEM{ts = NewTSList},
   % Save the updated metadata to ram and disk
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   % TO DO: replicate TS.dets on the new node
   {ok, [TS, NewTSList]}
 catch
    error:Error -> {error, [Error]}
 end.	 


% Remove TS from metadata
removeTS_(TS) -> 
 try
   % get current metadata
   {_, ClusterMetadata} = sbsystem:get_cluster_metadata(),
   % check wheter Tuple Space TS is already registered into Metadata
   TSisRegistered = lists:member(TS, [TSName || {TSName, _} <- ClusterMetadata?SYSTEM.ts]),
   if TSisRegistered == false -> 
      erlang:error(tuple_space_does_not_exist);
      true -> ok
   end,
   % Take TS data from Metadata
   OldTS = lists:nth(1, [{N,S} || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
   io:format("OldTS ~p~n", [OldTS]),
   % Take The list of nodes associated with TS and check if it's empty
   % since it's allowed to remove only TS without associated nodes
   %Nodes = lists:nth([S || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
   {_, Nodes} = OldTS,
   io:format("Nodes ~p~n", [Nodes]),
   case Nodes of
      [] -> ok;
       _ -> erlang:error(cant_remove_ts_with_nodes)
   end,
   % Remove old TS data from TSList
   NewTSList = lists:delete(OldTS, ClusterMetadata?SYSTEM.ts),
   % Store the updated metadata on a peg variable
   CMetaUpdated = ClusterMetadata?SYSTEM{ts = NewTSList},
   % Save the updated metadata to ram and disk
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   % TO DO: replicate TS.dets on the new node
   {ok, [NewTSList]}
 catch
    error:Error -> {error, [Error]}
 end.	 


nodes_(TS) -> 
 try
   % get current metadata
   {_, ClusterMetadata} = sbsystem:get_cluster_metadata(),
   % check wheter Tuple Space TS is already registered into Metadata
   TSisRegistered = lists:member(TS, [TSName || {TSName, _} <- ClusterMetadata?SYSTEM.ts]),
   if TSisRegistered == false -> 
      erlang:error(tuple_space_does_not_exist);
      true -> ok
   end,
   % Take The list of nodes associated with TS
   List = lists:nth(1, [S || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
   % io:format("List (node_): ~p~n", [List]),
   case List of
      [] -> {Ret, Nodes} = {no_associated_nodes, [no_nodes]};
       _ -> {Ret, Nodes} = {ok, List}
   end,
   % old: deleteme Nodes = lists:nth(1, [S || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
   {Ret, Nodes}
 catch
    error:Error -> {error, [Error]}
 end.	 




%gen_server's callbacks
start() ->
  % system_flag(trap_exit, true),
  % Note: the process is registered as global in order for every node to receive
  % the wake up message
  % Sending wake up message: {sbts, 'ra4@localhost'}!wake_up.
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init(_Args) -> {ok, ready}.

init() -> init(init).

stop() ->
   gen_server:cast(?MODULE, stop).	

% handle_call callback function
% NOTE: State is used to pass From to the outside
% thus State must not be changed otherwise
handle_call(Call, From, State) -> 
   case Call of
% Interface 1-2	   
      {new, Name}       -> {Ret, List} = new_(Name);
      {in, TS, Pattern} -> {Ret, List} = in_(TS, Pattern);
      {rd, TS, Pattern} -> {Ret, List} = rd_(TS, Pattern);
      {out, TS, Tuple}  -> {Ret, List} = out_(TS, Tuple);
% Interface 3
    {addNode, TS, Node} -> {Ret, List} = addNode_(TS, Node);
 {removeNode, TS, Node} -> {Ret, List} = removeNode_(TS, Node);
	    {nodes, TS} -> {Ret, List} = nodes_(TS);
         {removeTS, TS} -> {Ret, List} = removeTS_(TS);
	           halt -> {Ret, List} = {halt, []};
 	              _ -> {Ret, List} = {badarg, []}
   end,
   case {Ret, List} of
    {halt, _} -> {noreply, {no_match, From}};
      {_, []} -> {reply, {no_match, From}, State};   %{noreply, {no_match, From}};
       {_, _} -> {reply, {Ret, List}, State}
       %{_, _} -> {reply, List, State}
   end.


wake_up(From) ->
   io:format("Waking up after block. Ciao~n"),
   {no_match, F} = From,
   gen_server:reply(F, ok).


handle_info(Info, State) ->
   io:format("Got message ~p~n", [Info]),
   case Info of
      new_tuple_in ->
         case State of
            {no_match, _} -> wake_up(State);
                        _ -> ok
         end;
	        _ -> ok
   end,
   {noreply, State}. 


handle_cast(stop, State) -> {stop, normal, State}.

% Suspend execution with explicit call to methods

halt() ->
   gen_server:call(?MODULE, halt, infinity).

halt(Timeout) ->
   gen_server:call(?MODULE, halt, Timeout).


% Interface 1
new(Name) ->
   gen_server:call(?MODULE, {new, Name}).

in(TS, Pattern) ->
   gen_server:call(?MODULE, {in, TS, Pattern}, infinity).

rd(TS, Pattern) ->
   gen_server:call(?MODULE, {rd, TS, Pattern}, infinity).


in(TS, Pattern, Timeout) ->
   %gen_server:call(?MODULE, {in, TS, Pattern}, Timeout).
   try 
      Result = gen_server:call(?MODULE, {in, TS, Pattern}, Timeout),
      {ok, Result}
   catch Error:Reason -> 
      case Reason of 
         {timeout, _} -> {err, timeout};
	           _  -> {Error, Reason}
      end
   end.      


rd(TS, Pattern, Timeout) ->
   try 
      Result = gen_server:call(?MODULE, {rd, TS, Pattern}, Timeout),
      {ok, Result}
   catch Error:Reason -> 
      case Reason of 
         {timeout, _} -> {err, timeout};
	           _  -> {Error, Reason}
      end
   end.      


out(TS, Tuple) ->
   gen_server:call(?MODULE, {out, TS, Tuple}).

addNode(TS, Node) ->
   gen_server:call(?MODULE, {addNode, TS, Node}).

removeNode(TS, Node) ->
   gen_server:call(?MODULE, {removeNode, TS, Node}).

nodes(TS) ->
   gen_server:call(?MODULE, {nodes, TS}).

removeTS(TS) ->
   gen_server:call(?MODULE, {removeTS, TS}).
