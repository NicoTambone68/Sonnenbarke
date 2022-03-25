-module(sbenv).

-export([
	 get_cluster_env/1
	]).

% return value of corresponding Param read from sys.config
get_cluster_env(Param) ->
   % Read initial values from config/sys.config
   {ok,L} = application:get_env(system_metadata, cluster),
   case Param of
      name ->   [{name, ClusterName}] = lists:filter(fun({X,_}) -> X == name end, L),
		{ok, ClusterName};
      starting_scn -> [{starting_scn, StartingScn}] = lists:filter(fun({X,_}) -> X == starting_scn end, L),
		      {ok, StartingScn};
      ra_home_dir -> [{ra_home_dir, RaHomeDir}] = lists:filter(fun({X,_}) -> X == ra_home_dir end, L),
		     {ok, RaHomeDir};
      cluster_datafiles_home_dir -> [{cluster_datafiles_home_dir, CDHomeDir}] = lists:filter(fun({X,_}) -> X == cluster_datafiles_home_dir end, L),
				    {ok, CDHomeDir};
      nodes -> [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, L),
	       {ok, Nodes};
      _ -> {error, badarg}
   end.
