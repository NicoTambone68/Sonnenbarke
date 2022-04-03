-module(sb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    end_per_suite/2,
    groups/0,
    init_per_suite/2
]).

-export([ 
	sb_basic_test/1
	]).

% SUITE callbacks

all() -> [{group, public}].

groups() -> [
	     {public, [shuffle], [sb_basic_test]}
	    ].


init_per_suite(public, Config) ->
	Config;
init_per_suite(_, Config) -> Config.


end_per_suite(public, _) ->
	ok;
end_per_suite(_, _) -> ok.

% ////
%
% public group tests

sb_basic_test(_Config) ->
   sb:start_cluster(),
   sb:stop_cluster(),
   ok.

