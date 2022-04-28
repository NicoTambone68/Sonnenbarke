%% This Source Code Form is subject to the terms of the Apache License 2.0
%% Copyright (c) 2022 Nicolò Tambone
%%
%% @doc This module is for generating an unique integer
%% which is used by the cluster to mark every transaction with an Scn (System Change Number)

-module(sbcount).
-behaviour(gen_server).

-export([
   start/1,
   init/1,
   get_sequence/1,
   handle_call/3,
   handle_cast/2
]).


%% @doc Starts the gen_server
%%
%% @param _Args = term()
%%
%% @returns term()
%%
%% @end
-spec start(term()) -> term().
start(_Args) ->
  %% start the named gen server
  gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).

%% @doc Module initialization
%%
%% @param _Args = term()
%%
%% @returns
%%
%% @end
-spec init(term()) -> term().
init(_Args) ->
      % _Args must have the form [integer(), {integer(), integer()}, .
      %                           #counters, {Idx1, Init1}, {Idx2, Init2}, ...
      % the first argument of the _Args list is the number of counters
      case is_integer(lists:nth(1, _Args)) of
      true -> 
         % instantiate counters and get its reference		      
         Cref = counters:new(lists:nth(1, _Args), [atomics]),
	 % save the reference to a persistent term
         persistent_term:put({?MODULE, sequence}, Cref),
         % for every set of counters
	 % initialize the sequence to the starting number
	 [_|T] = _Args,
	 [counters:add(persistent_term:get({?MODULE, sequence}), Idx, Init-1) || {Idx, Init} <- T],
         {ok, initial_state()};
      false ->
         {bad_argument, initial_state()}
   end.

initial_state() -> 0.

%% @doc Get the sequence number. Idx is the index of the counter array 
%% Here we use just one 
%%
%% @param Idx = integer() 
%%
%% @returns integer()
%%
%% @end
-spec get_sequence(integer()) -> integer().
get_sequence(Idx) ->
   gen_server:call(?MODULE, {get_sequence, Idx}).

%% @doc Callback function of the gen_server
%% Gets the current value of the counter array @ Idx and adds one
%% Then return it
%%
%% @param {get_sequence, Idx}, term()
%%
%% @returns integer()
%%
%% @end
-spec handle_call(term(), term(), term()) -> term().
handle_call({get_sequence, Idx}, _From, _ ) ->
	%counters:add(persistent_term:get({?MODULE, sequence}), 1, 1),
	counters:add(persistent_term:get({?MODULE, sequence}), Idx, 1),
	%Scn = counters:get(persistent_term:get({?MODULE, sequence}), 1),
	Seq = counters:get(persistent_term:get({?MODULE, sequence}), Idx),
        {reply, Seq, initial_state()}.

-spec handle_cast(term(), term()) -> term().
handle_cast(_, _) ->
	{reply, ok}.
