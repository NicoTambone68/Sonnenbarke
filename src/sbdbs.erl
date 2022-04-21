%%
%%
%%
%% @doc The module for writing and reading data on database.

-module(sbdbs).
-include("sys_meta.hrl").

-export([
	 open_tables/0, 
	 close_tables/0,
	 open_table/1,
	 open_table/2,
	 close_table/1,
	 delete_table/1,
	 insert_ts/2,
	 lookup_table/2,
	 match_ts/2,
	 match_delete_ts/2,
	 scan_ts/1,
	 scan_ts/2,
	 traverse/1,
	 select_all/1,
         update_cluster_metadata/1,
         update_cluster_metadata/2,
	 get_cluster_metadata/1
	]).


%% @doc Opens the cluster metadata's table  
%% by means of the configuration parameters config/sys.config
%% Also replicates the same data structures in memory
%%
%% @param none
%%
%% @returns none
%%
%% @end
%%
-spec open_tables() -> term().
open_tables() ->
   % gets the datafile home dir from the configurations parameter
   {ok, CDHomeDir} = sbenv:get_cluster_env(cluster_datafiles_home_dir),
   FileName = string:join([CDHomeDir, atom_to_list(node()), "sys_meta.dets"], "/"),
   SysRam = ets:whereis(sysRam),
   if SysRam == undefined ->
      ets:new(sysRam, [ordered_set, public, named_table, {keypos, #sys_meta.id}]);
      true -> sys_ram_already_open
   end,
   SysIndex = ets:whereis(sysIndex),
   if SysIndex == undefined ->
      ets:new(sysIndex, [ordered_set, public, named_table]);
      true -> sys_index_already_open
   end,
   dets:open_file(sysDisk, [{file, FileName}, {type, set}, {keypos, #sys_meta.id}]).


%% @doc Closes the cluster's metadata tables on ram and disk 
%%
%% @param none
%%
%% @returns ok|{error, Reason}
%%
%% @end
%%
-spec close_tables() -> term().
close_tables() ->
   try
      dets:sync(sysDisk),	   
      dets:close(sysDisk),
      ets:delete(sysRam),
      ets:delete(sysIndex),
      ok
   catch
      error:Reason -> {error, Reason}
   end.


%% @doc Opens a Tuple Space's datafile. Optionally creates it if it doesn't exist
%% 
%%
%% @param TSName = name of the Tuple Space, Args = none | create_if_not_exists
%%
%% @returns {ok, TSName} | {err, file_not_found}
%%
%% @end
%%
-spec open_table(string(), tuple()) -> term().
open_table(TSName, Args) ->
   % build the following filename: .ra/<current_node>/sys_meta.dets	
   {ok, CDHomeDir} = sbenv:get_cluster_env(cluster_datafiles_home_dir),
   FName = atom_to_list(TSName) ++ atom_to_list('.dets'),
   FileName = string:join([CDHomeDir, atom_to_list(node()), FName], "/"),
   case filelib:is_file(FileName) of 
      true ->
         dets:open_file(TSName, [{file, FileName}, {type, set}]),
	 {ok, TSName};
      false ->
         case Args of 
            create_if_not_exists ->
               dets:open_file(TSName, [{file, FileName}, {type, set}]),
	       {ok, TSName};
            _ -> erlang:error(file_not_found)
         end
   end.

%% @doc Opens a TS datafile TSName.  
%%
%% @param TSName
%%
%% @returns {ok, TSName} | {err, file_not_found}
%%
%% @end
%%
-spec open_table(string()) -> term().
open_table(TSName) ->
   ?MODULE:open_table(TSName, error_if_not_exists).

%% @doc Closes the TSName's datafile 
%%
%% @param TSName 
%%
%% @returns ok | {error, Reason}
%%
%% @end
%%
-spec close_table(string()) -> term().
close_table(TSName) ->
   %dets:sync(TSName),
   dets:close(TSName).

%% @doc phisically deletes TSName's datafiles 
%%
%% @param TSName
%%
%% @returns ok | {ok, TSName}
%%
%% @end
%%
-spec delete_table(string()) -> term().
delete_table(TSName) ->
   % build the following filename: .ra/<current_node>/sys_meta.dets	
   {ok, CDHomeDir} = sbenv:get_cluster_env(cluster_datafiles_home_dir),
   FName = atom_to_list(TSName) ++ atom_to_list('.dets'),
   FileName = string:join([CDHomeDir, atom_to_list(node()), FName], "/"),
   case filelib:is_file(FileName) of 
      true ->
         dets:close(TSName),
	 file:delete(FileName),
	 {ok, TSName};
      false -> ok
   end.

%% @doc Insert Value into the Tuple Space TSName
%%
%% @param TSName = string(), Value = tuple()
%%
%% @returns {ok, Value} | {error, Error}
%%
%% @end
%%
-spec insert_ts(string(), tuple()) -> term().
insert_ts(TSName, Value) ->
   try	
      ?MODULE:open_table(TSName),
      dets:insert(TSName, Value),
      dets:sync(TSName),
      ?MODULE:close_table(TSName),
      {ok, Value}
   catch
      error:Error -> {error, Error}
   end.

%% @doc Searches TSName for a tuple matching Pattern
%%
%% @param TSName = string(), Pattern = tuple()
%%
%% @returns {ok, [Pattern]} | {error, Reason}
%%
%% @end
%%
-spec lookup_table(string(), tuple()) -> term().
lookup_table(TSName, Pattern) ->
  try
   ?MODULE:open_table(TSName),
   EtsRet  = dets:lookup(TSName, Pattern),
   ?MODULE:close_table(TSName),
   {ok, EtsRet}
  catch
     error:Error -> {error, Error}
  end.

%% @doc Returns one or more tuples matching the pattern Pattern 
%%
%% @param TSName = string(), Pattern = tuple()
%%
%% @returns {ok, [tuple()]} | {error, Reason}
%%
%% @end
%%
-spec match_ts(string(), tuple()) -> term().
match_ts(TSName, Pattern) ->
  try
   ?MODULE:open_table(TSName),
   EtsRet  = dets:match_object(TSName, Pattern),
   ?MODULE:close_table(TSName),
   {ok, EtsRet}
  catch
     error:Error -> {error, Error}
  end.

%% @doc Returns one or more tuples matching the pattern Pattern 
%% Deletes the matching pattern from the TS
%%
%% @param TSName = string(), Pattern = tuple()
%%
%% @returns {ok, [tuple()]} | {error, Reason}
%%
%% @end
%%
-spec match_delete_ts(string(), tuple()) -> term().
match_delete_ts(TSName, Pattern) ->
  try
   {_, Ret} = ?MODULE:match_ts(TSName, Pattern),
   ?MODULE:open_table(TSName),
   dets:match_delete(TSName, Pattern),
   dets:sync(TSName),
   ?MODULE:close_table(TSName),
   {ok, Ret}
  catch
     error:Error -> {error, Error}
  end.

%% @doc Opens the datafile of the Tuple Space TSName
%% Extracts the first record and the associated key
%%
%% @param TSName = string()
%%
%% @returns {Result, Key}|{error, Error}
%%
%% @end
%%
-spec scan_ts(string()) -> term().
scan_ts(TSName) ->
   try
      ?MODULE:open_table(TSName),
      Key = dets:first(TSName),
      Result = dets:lookup(TSName, Key),
      {Result, Key}
   catch
      error:Error -> {error, Error}
   end.

%% @doc Opens the datafile of the Tuple Space TSName
%% Reads the record associated with KeyNext, 
%% which is the key following Key
%% Return the given record and KeyNext
%% @param TSName = string(), Key = term()
%%
%% @returns {Result, Key}|{error, Error}
%%
%% @end
%%
-spec scan_ts(string(), term()) -> term().
scan_ts(TSName, Key) ->
   try
      KeyNext = dets:next(TSName, Key),
      Result = dets:lookup(TSName, KeyNext),
      %io:format("~p~n", [Result]),
      case KeyNext of
        '$end_of_table' -> {Result, ok};
                     _  -> {Result, KeyNext}
      end
   catch
      error:Error -> {error, Error}
   end.

%% @doc Prints out all the records of a TS 
%% for debug purposes
%%
%% @param TSName = string()
%%
%% @returns [tuple()]|{error, Error}
%%
%% @end
%%
-spec traverse(string()) -> term().
traverse(TSName) ->
   try
      ?MODULE:open_table(TSName),
      dets:traverse(TSName, fun(X) -> io:format("~p~n", [X]), continue end),
      ?MODULE:close_table(TSName)
   catch
      error:Error -> {error, Error}
   end.

%% @doc Gets all the records of a TS in a list
%% for debug purposes
%%
%% @param TSName = string()
%%
%% @returns [tuple()]|{error, Error}
%%
%% @end
%%
-spec select_all(string()) -> term().
select_all(TSName) ->
   try
      ?MODULE:open_table(TSName),
      List = dets:traverse(TSName, fun(X) -> {continue, X} end),
      ?MODULE:close_table(TSName),
      List
   catch
      error:Error -> {error, Error}
   end.



%% @doc Updates cluster's metadata with the given data
%%
%% @param Cluster
%%
%% @returns ok
%%
%% @end
%%
-spec update_cluster_metadata(term()) -> term().
update_cluster_metadata(Cluster) ->
	ets:insert(sysRam, Cluster),
	dets:insert(sysDisk, Cluster),
	dets:sync(sysDisk),
	ok.

%% @doc Updates cluster's metadata with the given data
%%
%% @param Cluster, Mode = ram | disk | both    
%%
%% @returns {ok, Cluster}
%%
%% @end
%%
-spec update_cluster_metadata(term(), atom()) -> term().
update_cluster_metadata(Cluster, Mode) ->
   case Mode of
      ram -> ets:insert(sysRam, Cluster);
     disk -> dets:insert(sysDisk, Cluster),
	     dets:sync(sysDisk);
     both -> update_cluster_metadata(Cluster)
   end,
   {ok, Cluster}.


%% @doc gets the current cluster's metadata 
%%
%% @param From = ram | disk
%%
%% @returns {ok, Cluster} | {error, no_data}
%%
%% @end
%%
-spec get_cluster_metadata(term()) -> term().
get_cluster_metadata(From) ->
   case From of
      ram ->	   
	case ets:lookup(sysRam, 1) of
		[Cluster] -> {ok, Cluster};
		[] -> {error, no_data}
	end;
      disk ->
	case dets:lookup(sysDisk, 1) of
		[Cluster] -> {ok, Cluster};
		[] -> {error, no_data}
	end
   end.

