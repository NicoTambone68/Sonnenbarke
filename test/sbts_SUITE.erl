-module(sbts_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    end_per_suite/2,
    groups/0,
    init_per_suite/2
]).

-export([ 
	rd_test/1
	]).

% SUITE callbacks

all() -> [{group, public}].

groups() -> [
	     {public, [shuffle], [rd_test]}
	    ].


init_per_suite(public, Config) ->
	sbsystem:start(),
	sbts:start(),
	Config;
init_per_suite(_, Config) -> Config.


end_per_suite(public, _) ->
	sbts:stop(),
	sbsystem:stop(),
	ok;
end_per_suite(_, _) -> ok.

% ////
%
% public group tests

rd_test(_Config) ->
	gen_server:start({local, sbts}, sbts, [], []),
	sbts:rd(minnie,{any,any}),
        sbts:rd(minnie,{pippo,pluto}),
	ok.
