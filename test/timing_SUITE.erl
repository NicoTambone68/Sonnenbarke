-module(timing_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("sys_meta.hrl").

-export([
    all/0,
    end_per_suite/2,
    groups/0,
    init_per_suite/2
]).

-export([ 
%         timing_test/1
	 test_1/1,
	 test_2/1
	]).

% SUITE callbacks

all() -> [{group, public}].

groups() -> [
	     {public, [shuffle], [
				  test_2
				 ]}

	    ].

init_per_suite(public, Config) -> Config;
init_per_suite(_, Config) -> Config.


end_per_suite(public, _) -> ok;
end_per_suite(_, _) -> ok.


test_1(_Config) ->
 sbcli:create_cluster_metadata(),
 timing_test(10),
 sbcli:create_cluster_metadata(),
 timing_test(100),
 sbcli:create_cluster_metadata(),
 timing_test(1000),
 sbcli:create_cluster_metadata(),
 timing_test(10000).

test_2(_Config) ->
 sbcli:create_cluster_metadata(),
 timing_test2(10).

timing_test2(Cardinality) ->

   sbcli:start(),

   timer:sleep(2000),
   {ok, Ln} = application:get_env(system_metadata, cluster),
   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, Ln),
   [{cluster_datafiles_home_dir, Dir}] = lists:filter(fun({X,_}) -> X == cluster_datafiles_home_dir end, Ln),

   FName = Dir ++ "/" ++ integer_to_list(Cardinality) ++ "_timing2.txt",
   
   {ok, File} = file:open(FName, [write]),
   
   TSName = adcc_test_timing,
   
   sbcli:new(TSName),
   L = [{N, rand:uniform(1000)} || N <- lists:seq(1, Cardinality)],

   [sbcli:addNode(TSName, N) || N <- Nodes],

   % get average time for function out
   get_avg_time_fun(out, TSName, L, File),

   % get average time for function rd  
   get_avg_time_fun(rd, TSName, L, File),

   % get average time for function in  
   get_avg_time_fun(in, TSName, L, File),

   file:close(File),

sbcli:stop(),
ok.



timing_test(Cardinality) ->

   sbcli:start(),

   timer:sleep(2000),
   
   % get the list of cluster nodes
   {ok, Ln} = application:get_env(system_metadata, cluster),
   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, Ln),
   [{cluster_datafiles_home_dir, Dir}] = lists:filter(fun({X,_}) -> X == cluster_datafiles_home_dir end, Ln),

   TSName = adcc_test_timing,
   % clear the test tuple space
   sbcli:removeNode(TSName, node()),

   % TO DO: refactor
   Count = Cardinality,

   %{ok, Cwd} = file:get_cwd(),

   FName = Dir ++ "/" ++ integer_to_list(Cardinality) ++ "_timing.txt",

   {ok, File} = file:open(FName, [write]),
   
   io:format(File, "Cardinality of test elements: ~p~n" ,[Count]),

   TSNames =  ["adcc_" ++ integer_to_list(N) || N <- lists:seq(1, Count)],

   % New TS
   RNew =  [timer:tc(sbcli, new, [list_to_atom(N)]) || N <- TSNames],

   AvgTime = lists:foldl(fun(X, Sum)-> X + Sum end, 0, [Timing || {Timing, _} <- RNew])/Count,

   io:format(File, "Average time for operation new ~p~n" ,[AvgTime]),

   % Remove the nodes and the TS
  
   RRemove = [timer:tc(sbcli, removeNode, [list_to_atom(N), node()]) || N <- TSNames],
   
   AvgTimeRemove = lists:foldl(fun(X, Sum)-> X + Sum end, 0, [TimingRemove || {TimingRemove, _} <- RRemove])/Count,
   
   io:format(File, "Average time for operation removeNode ~p~n" ,[AvgTimeRemove]),


   sbcli:new(TSName),
   L = [{N, rand:uniform(1000)} || N <- lists:seq(1, Count)],

   % get average time for function out
   get_avg_time_fun(out, TSName, L, File),

   % get average tim for function addNode
   get_avg_time_fun(addNode, TSName, Nodes, File),

   % get average time for function rd  
   get_avg_time_fun(rd, TSName, L, File),

   % get average time for function in  
   get_avg_time_fun(in, TSName, L, File),

   file:close(File),

sbcli:stop(),
ok.



% Time in microseconds
get_avg_time_fun(F, TS, List, File) ->

   Count = length(List),

   case F of
      Func when Func =:= out; Func =:= addNode  -> Response =  [timer:tc(sbcli, F, [TS, N]) || N <- List];
	     _ -> Response =  [timer:tc(sbcli, F, [TS, N, 500]) || N <- List]
   end,
   
   AvgTime = lists:foldl(fun(X, Sum)-> X + Sum end, 0, [Timing || {Timing, _} <- Response])/Count,

   io:format(File, "Average time for operation ~p ~p~n" , [F, AvgTime]).


   

%reset_metadata() ->
%   {ok,L} = application:get_env(system_metadata, cluster),
%   [{cluster_datafiles_home_dir, Dir}] = lists:filter(fun({X,_}) -> X == cluster_datafiles_home_dir end, L),
%   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, L),
%   [file:del_dir_r(Dir ++ "/" ++ atom_to_list(N)) || N <- Nodes],
%   sbcli:create_cluster_metadata(),
%   timer:sleep(1000).


