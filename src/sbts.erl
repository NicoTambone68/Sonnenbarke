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
	wake_up/1,

	% interface 1
	new/1,
	in/2,
	rd/2,
	% delete this (only for test)
%	rd_/2,
	out/2,
	match/1,

	% interface 2
	in/3,
	rd/3,

	% interface 3
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
new_(Name) -> 
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
   sbdbs:insert_table(TS, Tuple),
   {ok, [Tuple]}.

% Interface 2/3
% Tima out in API Call (see below)

% Interface 3/3
addNode_(TS, Node) -> ok.
removeNode_(TS, Node) -> ok.
nodes_(TS) -> ok.

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
      {new, Name}       -> {ok, List} = new_(Name);
      {in, TS, Pattern} -> {ok, List} = in_(TS, Pattern);
      {rd, TS, Pattern} -> {ok, List} = rd_(TS, Pattern);
      {out, TS, Tuple}  -> {ok, List} = out_(TS, Tuple);
 	              _ -> {badarg, List} = {badarg, []}
   end,
   case List of
      [] -> {noreply, {no_match, From}};
       _ -> {reply, List, State}
   end.


wake_up(From) ->
   io:format("Waking up after hibernation. Ciao~n"),
   {no_match, F} = From,
   gen_server:reply(F, [ok]).


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




% Interface 1
new(Name) ->
   gen_server:call(?MODULE, {new, Name}).

in(TS, Pattern) ->
   gen_server:call(?MODULE, {in, TS, Pattern}).

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
