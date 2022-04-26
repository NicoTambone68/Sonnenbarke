%%
%%
%%
%% @doc This module is for handling all of the tuple space methods

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
	new/2,
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
	copy_ts/3,
	copy_ts/4,
	
	% private funcs
	addNode_/2,
	removeNode_/2,
	removeTS_/1,
	nodes_/1
	]).

-define(SYSTEM, #sys_meta).

%% @doc Registers the new TS Name on node Node
%%
%% @param Name = string(), Node = atom()
%%
%% @returns term()
%%
%% @end
-spec new_(string(), atom()) -> term().
new_(Name, Node) -> 
   % get current metadata
   {_, ClusterMetadata} = sbsystem:get_cluster_metadata(),

   % Chek if Name Node already exist
   TSList = ClusterMetadata?SYSTEM.ts,
   % Get all TSNames from TSList
   case lists:member(Name, [N || {N,_} <- TSList]) of
	   % Name is already there. Check the associated nodes
	   true -> [{Name, AN}] = [{N,L} || {N,L} <- TSList, N == Name],
                   case lists:member(Node, AN) of
                       % Name and node already there. nothing to do.
                       true -> Action = nothing_to_do;
                      % Name is already there, but not the node
                      false -> Action = add_node_only
		   end;
           % Name and node must be inserted
	   false -> Action = add_name_and_node
   end,

   case Action of
      add_name_and_node -> TS = [{Name, [Node]} | ClusterMetadata?SYSTEM.ts],
                           % create dets file
                           sbdbs:open_table(Name, create_if_not_exists),
                           % close 
                           sbdbs:close_table(Name),
			   Return = {ok, [Name, Node]};

          add_node_only -> [{Name, ANodes}] = [{N,L} || {N,L} <- TSList, N == Name],
		           TS = [{Name, [Node|ANodes]} | lists:delete({Name, ANodes}, ClusterMetadata?SYSTEM.ts)],
                           % create dets file
                           sbdbs:open_table(Name, create_if_not_exists),
                           % close 
                           sbdbs:close_table(Name),
			   Return = {ok, [Name, Node]};

          nothing_to_do -> TS = ClusterMetadata?SYSTEM.ts,
		           Return = {ok, [ts_already_exists]}
   end,

   % Update Cluster Metadata 
   % Store the updated metadata on a peg variable
   CMetaUpdated = ClusterMetadata?SYSTEM{ts = TS},
   % Save the updated metadata to ram and disk
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   Return.


%% @doc Implements the in method 
%%
%% @param TS = string(), Pattern = tuple()
%%
%% @returns term()
%%
%% @end
-spec in_(string(), tuple()) -> term().
in_(TS, Pattern) -> 
   %MatchPattern = ?MODULE:match(Pattern), 
   sbdbs:match_delete_ts(TS, ?MODULE:match(Pattern)).

%% @doc Implements the rd method
%%
%% @param TS = string(), Pattern = tuple()
%%
%% @returns term()
%%
%% @end
-spec rd_(string(), tuple()) -> term().
rd_(TS, Pattern) -> 
   % MatchPattern = ?MODULE:match(Pattern), 
   sbdbs:match_ts(TS, ?MODULE:match(Pattern)).

%% @doc Translates the InputPattern with wildcards (any) to a pattern which 
%% actually performs dets:match_object
%%
%% @param InputPattern = tuple() 
%%
%% @returns term()
%%
%% @end
-spec match(tuple()) -> term().
match(InputPattern) ->
   List = tuple_to_list(InputPattern),
   list_to_tuple(lists:map(fun(X) -> case X of any -> '_'; _ -> X end end, List)).

%% @doc Implements the out method 
%%
%% @param TS = string(), Tuple = tuple() 
%%
%% @returns term()
%%
%% @end
-spec out_(string(), tuple()) -> term().
out_(TS, Tuple) ->
      sbdbs:insert_ts(TS, Tuple).

% Interface 2/3
% Time out in API Call (see below)

% Interface 3/3
%
%% @doc Implements addNode
%%
%% @param TS = string(), Node = atom()
%%
%% @returns term()
%%
%% @end
%%
-spec addNode_(string(), atom()) -> term().
addNode_(TS, Node) -> 
     {_, Nodes} = ?MODULE:nodes(TS),
     case Nodes of
        [tuple_space_does_not_exist] ->
           ?MODULE:new(TS, Node),
           {ok, [Node]};
                     
         % TS already on cluster. Add Node if not a member of the TS
	 _ -> case lists:member(Node, Nodes) of
                 true -> {ok, [Node]};

		false -> ?MODULE:new(TS, Node),
			 % Note that the new node is added to the Head of the list
			 % so take care to pick the last element to copy from
                         NodeFrom = lists:last(Nodes),
                         ?MODULE:copy_ts(TS, NodeFrom, Node),
			 {ok, [Node]}
	      end 
     end.

%% TO DO:remove
%%
%-spec addNode_(string(), atom()) -> term().
%addNode_(TS, Node) -> 
% try
%   % get current metadata
%   {_, ClusterMetadata} = sbsystem:get_cluster_metadata(),
%   % check wheter Node belongs to the cluster or not
%   NodeIsRegistered = lists:member(Node, ClusterMetadata?SYSTEM.nodes), 
%   if NodeIsRegistered == false -> 
%      erlang:error(not_a_cluster_node);
%      true -> ok
%   end,
%   % check wheter Tuple Space TS is already registered into Metadata
%   TSisRegistered = lists:member(TS, [TSName || {TSName, _} <- ClusterMetadata?SYSTEM.ts]),
%   if TSisRegistered == false -> 
%      erlang:error(tuple_space_does_not_exist);
%      true -> ok
%   end,
%   % Take TS data from Metadata
%   OldTS = lists:nth(1, [{N,S} || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
%   % Take The list of nodes associated with TS
%   Nodes = lists:nth(1, [S || {N,S} <- ClusterMetadata?SYSTEM.ts, N == TS]),
%   % Add the new list of nodes to the TS
%   % TS = [{Name, [Leader]} | ClusterMetadata?SYSTEM.ts],
%   NewTS = {TS, [Node | Nodes]}, %{TS, lists:append(Nodes, Node)},
%   % Remove old TS data from TSList
%   TSList = lists:delete(OldTS, ClusterMetadata?SYSTEM.ts),
%   % Add the tuple {TS,[Nodes]} to the TSList
%   NewTSList = [NewTS | TSList], %lists:append(TSList, NewTS),
%   % Store the updated metadata on a peg variable
%   CMetaUpdated = ClusterMetadata?SYSTEM{ts = NewTSList},
%   % Save the updated metadata to ram and disk
%   sbdbs:update_cluster_metadata(CMetaUpdated, both),
%   % TO DO: replicate TS.dets on the new node
%   {ok, [TS, NewTSList]}
% catch
%    error:Error -> {error, Error}
% end.	 

%% @doc Implements removeNode
%%
%% @param TS = string(), Node = atom()
%%
%% @returns term()
%%
%% @end
-spec removeNode_(string(), atom()) -> term().
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
   {ok, [TS, NewTSList]}
 catch
    error:Error -> {error, [Error]}
 end.	 

%% @doc Remove TS from metadata 
%%
%% @param TS = string()
%%
%% @returns term()
%%
%% @end
-spec removeTS_(string()) -> term().
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

%% @doc Implements nodes(TS)
%%
%% @param TS = string()
%%
%% @returns term()
%%
%% @end
-spec nodes_(string()) -> term().
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



%% @doc Starts the gen_server's process
%%
%% @param none
%%
%%
%% @returns term()
%%
%% @end
-spec start() -> term().
start() ->
  % Note: the process is registered as global in order for every node to receive
  % the wake up message
  % Sending wake up message: {sbts, 'ra4@localhost'}!wake_up.
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initializes the gen_server
%%
%% @param 
%%
%% @returns
%%
%% @end
-spec init(term()) -> term().
init(_Args) -> {ok, ready}.

-spec init() -> term().
init() -> init(init).

%% @doc Stops the gen_server
%%
%% @param 
%%
%% @returns
%%
%% @end
-spec stop() -> term().
stop() ->
   gen_server:cast(?MODULE, stop).	

%% @doc HandleCall callback function
%% NOTE: State is used to pass From to the outside
%% thus State must not be changed otherwise
%%
%% @param Call = term(), From = term(), State = term()
%%
%% @returns term()
%%
%% @end
-spec handle_call(term(), term(), term()) -> term().
handle_call(Call, From, State) -> 
   case Call of
% Interface 1-2	   
      {new, Name, Node} -> {Ret, List} = new_(Name, Node);
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


%% @doc Handles the wake-up call after blocking, replying to 
%% the caller of a gen_server method 
%%
%% @param From = term()
%%
%% @returns term()
%%
%% @end
-spec wake_up(term()) -> term().
wake_up(From) ->
   io:format("Waking up after block. Ciao~n"),
   {no_match, F} = From,
   gen_server:reply(F, ok).

%% @doc Manages the incoming messages and takes action accordingly.
%% We will catch new_tuple_in message to trigger the wake_up function
%%
%% @param Info = term(), State = term()
%%
%% @returns term()
%%
%% @end
-spec handle_info(term(), term()) -> term().
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

%% @doc Callback for the stop call
%%
%% @param stop = atom(), State = term()
%%
%% @returns term()
%%
%% @end
-spec handle_cast(atom(), term()) -> term().
%handle_cast(stop, State) -> {stop, normal, State}.

handle_cast(Call, State) ->
   case Call of
      {removeNode, TS, Node} -> {Ret, List} = removeNode_(TS, Node);
         {addNode, TS, Node} -> {Ret, List} = addNode_(TS, Node);	   
                       stop  -> {Ret, List} = {stop, []}
   end,
   case {Ret, List} of   
      {stop, []} -> {stop, normal, State}; 
      {_, _} -> {noreply, State}	   
   end.	   


% Suspend execution with explicit call to methods

%% @doc Implements the reqired block when there's no matching with methods in and rd.
%% This is done by calling the method halt with a time out of infinity. 
%% Inside the handle_call the function halt is not going to reply, thus causing 
%% the required block. Whenever a new_tuple_in message is received, a reply is 
%% given to the caller, thus removing the block. For further details, see
%% the above function wake_up
%%
%% @param none
%%
%% @returns term().
%%
%% @end
-spec halt() -> term().
halt() ->
   gen_server:call(?MODULE, halt, infinity).

%% @doc Implements halt with a timeout
%%
%% @param none
%%
%% @returns term()
%%
%% @end
-spec halt(integer()) -> term().
halt(Timeout) ->
   gen_server:call(?MODULE, halt, Timeout).


% Interface 1


%% @doc new
%%
%% @param Name = string(), Node = atom()
%%
%% @returns term()
%%
%% @end
-spec new(string(), atom()) -> term().
new(Name, Node) ->
   gen_server:call(?MODULE, {new, Name, Node}).

%% @doc in
%%
%% @param TS = string, Pattern = tuple()
%%
%% @returns
%%
%% @end
-spec in(string(), tuple()) -> term().
in(TS, Pattern) ->
   gen_server:call(?MODULE, {in, TS, Pattern}, infinity).

%% @doc rd
%%
%% @param TS = string, Pattern = tuple()
%%
%% @returns term()
%%
%% @end
-spec rd(string(), tuple()) -> term().
rd(TS, Pattern) ->
   gen_server:call(?MODULE, {rd, TS, Pattern}, infinity).

%% @doc in with Timeout 
%%
%% @param TS = string, Pattern = tuple()
%%
%% @returns term()
%%
%% @end
-spec in(string(), tuple(), integer()) -> term().
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

%% @doc rd with Timeout 
%%
%% @param TS = string, Pattern = tuple()
%%
%% @returns
%%
%% @end
-spec rd(string(), tuple(), integer()) -> term().
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

%% @doc out
%%
%% @param TS = string, Tuple = tuple()
%%
%% @returns term()
%%
%% @end
-spec out(string(), tuple()) -> term().
out(TS, Tuple) ->
   gen_server:call(?MODULE, {out, TS, Tuple}).

%% @doc addNode
%%
%% @param TS = string, Tuple = tuple()
%%
%% @returns term()
%%
%% @end
-spec addNode(string(), atom()) -> term().
addNode(TS, Node) ->
   gen_server:call(?MODULE, {addNode, TS, Node}).

%% @doc removeNode
%%
%% @param TS = string(), Tuple = tuple()
%%
%% @returns term().
%%
%% @end
-spec removeNode(string(), atom()) -> term().
removeNode(TS, Node) ->
   gen_server:call(?MODULE, {removeNode, TS, Node}).

%% @doc nodes
%%
%% @param TS = string()
%%
%% @returns term()
%%
%% @end
-spec nodes(string()) -> term().
nodes(TS) ->
   gen_server:call(?MODULE, {nodes, TS}).

%% @doc removeTS 
%%
%% @param TS = string()
%%
%% @returns term()
%%
%% @end
-spec removeTS(string()) -> term().
removeTS(TS) ->
   gen_server:call(?MODULE, {removeTS, TS}).

%% @doc Copies TS data from NodeFrom to NodeTo
%%
%% @param Tuple Space Name, NodeFrom, NodeTo 
%%
%% @returns ok
%%
%% @end
%%
-spec copy_ts(string(), atom(), atom()) -> term().
copy_ts(TS, NodeFrom, NodeTo) ->
   erpc:call(NodeFrom, sbdbs, open_table, [TS]),
   erpc:call(NodeTo, sbdbs, open_table, [TS, create_if_not_exists]),
   {List, Key} = erpc:call(NodeFrom, sbdbs, scan_ts, [TS]),
   case List of
      % TS is empty. Done.
      [] -> erpc:call(NodeFrom, sbdbs, close_table, [TS]),
	    erpc:call(NodeTo, sbdbs, close_table, [TS]),
	    ok;
      % TS is not empty. Copy the first record to the NodeTO
      _  -> erpc:call(NodeTo, sbts, out, [TS, lists:nth(1,List)]),
	    % Repeat recursively
	    copy_ts(TS, NodeFrom, NodeTo, Key)
   end.

-spec copy_ts(string(), atom(), atom(), term()) -> term().
copy_ts(TS, NodeFrom, NodeTo, Key) ->
   {NextList, NextKey} = erpc:call(NodeFrom, sbdbs, scan_ts, [TS, Key]),
   case NextList of
      % TS is empty. Done
      [] -> erpc:call(NodeFrom, sbdbs, close_table, [TS]),
	    erpc:call(NodeTo, sbdbs, close_table, [TS]),
	    ok;
      % TS is not empty. Copy Tuple to Node To
      _  -> erpc:call(NodeTo, sbts, out, [TS, lists:nth(1, NextList)]),
	    % Repeat recursively
	    copy_ts(TS, NodeFrom, NodeTo, NextKey)
   end.
