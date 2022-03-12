%%%-------------------------------------------------------------------
%% @doc sonnenbarke public API
%% @end
%%%-------------------------------------------------------------------

-module(sonnenbarke_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sonnenbarke_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
