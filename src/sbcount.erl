-module(sbcount).
-behaviour(gen_server).

-export([
   start/1,
   init/1,
   get_sequence/1,
   handle_call/3,
   handle_cast/2
]).


start(_Args) ->
  %% start the named gen server
  gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).


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

% Idx is the index of the counter array
get_sequence(Idx) ->
%   gen_server:call(?MODULE, get_sequence).
   gen_server:call(?MODULE, {get_sequence, Idx}).


handle_call({get_sequence, Idx}, _From, _ ) ->
	%counters:add(persistent_term:get({?MODULE, sequence}), 1, 1),
	counters:add(persistent_term:get({?MODULE, sequence}), Idx, 1),
	%Scn = counters:get(persistent_term:get({?MODULE, sequence}), 1),
	Seq = counters:get(persistent_term:get({?MODULE, sequence}), Idx),
        {reply, Seq, initial_state()}.


handle_cast(_, _) ->
	{reply, ok}.
