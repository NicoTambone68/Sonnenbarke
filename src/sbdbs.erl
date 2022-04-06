-module(sbdbs).
-include("sys_meta.hrl").

% TO DO: rename _table to _ts
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
         update_cluster_metadata/1,
         update_cluster_metadata/2,
	 get_cluster_metadata/1
	]).

% TO DO: read from config file
% -define(RA_HOME_DIR, "./.ra").
% TO DO: replace cabled filename w define
-define(SYSTEM_TAB_FILENAME, "sys_meta.dets").


open_tables() ->
   % build the following filename: .ra/<current_node>/sys_meta.dets	
   {ok, CDHomeDir} = sbenv:get_cluster_env(cluster_datafiles_home_dir),
   % FileName = string:join([?RA_HOME_DIR, atom_to_list(node()), "sys_meta.dets"], "/"),
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

% TO DO: Scan Metadata.TS and close all the TS
close_tables() ->
   ets:delete(sysRam),
   ets:delete(sysIndex),
   dets:close(sysDisk).


% Create Tuple Space TSName
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


open_table(TSName) ->
   ?MODULE:open_table(TSName, error_if_not_exists).

% To simplify things we are going to manage only disk tables here
%   TsRam = ets:whereis(TSName),
%   if TsRam == undefined ->
%      ets:new(TSName, [ordered_set, public, named_table]);
%      true -> ts_ram_already_open
%   end,
%   dets:open_file(TSName, [{file, FileName}, {type, set}]).


close_table(TSName) ->
   %ets:delete(TSName),
   dets:close(TSName).

% phisically deletes datafiles
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


insert_ts(TSName, Value) ->
   try	
      ?MODULE:open_table(TSName),
      dets:insert(TSName, Value),
      ?MODULE:close_table(TSName),
      {ok, Value}
   catch
      error:Error -> {error, Error}
   end.


lookup_table(TSName, Pattern) ->
  try
   ?MODULE:open_table(TSName),
   EtsRet  = dets:lookup(TSName, Pattern),
   % TO DO: load dets into ets at first lookup
   %DetsRet = ets:lookup(TSName, Value),
   ?MODULE:close_table(TSName),
   {ok, EtsRet}
  catch
     error:Error -> {error, Error}
  end.

% return one or more tuples matching the pattern Pattern
match_ts(TSName, Pattern) ->
  try
   ?MODULE:open_table(TSName),
   EtsRet  = dets:match_object(TSName, Pattern),
   ?MODULE:close_table(TSName),
   {ok, EtsRet}
  catch
     error:Error -> {error, Error}
  end.

% return one or more tuples matching the pattern Pattern
% delete the matching pattern from the TS
match_delete_ts(TSName, Pattern) ->
  try
   {_, Ret} = ?MODULE:match_ts(TSName, Pattern),
   ?MODULE:open_table(TSName),
   dets:match_delete(TSName, Pattern),
   ?MODULE:close_table(TSName),
   {ok, Ret}
  catch
     error:Error -> {error, Error}
  end.


scan_ts(TSName) ->
   try
      ?MODULE:open_table(TSName),
      Key = dets:first(TSName),
      Result = dets:lookup(TSName, Key),
      %io:format("~p~n", [Result]),
      %{Result, scan_ts(TSName, Key)}.
      {Result, Key}
   catch
      error:Error -> {error, Error}
   end.

scan_ts(TSName, Key) ->
   try
      KeyNext = dets:next(TSName, Key),
      Result = dets:lookup(TSName, KeyNext),
      %io:format("~p~n", [Result]),
      case KeyNext of
        '$end_of_table' -> {Result, ok};
                     _  -> {Result, KeyNext}
     		% {Result, scan_ts(TSName, KeyNext)}
      end
   catch
      error:Error -> {error, Error}
   end.

% for debug
% print out all the content of a TS
traverse(TSName) ->
   try
      ?MODULE:open_table(TSName),
      dets:traverse(TSName, fun(X) -> io:format("~p~n", [X]), continue end),
      ?MODULE:close_table(TSName)
   catch
      error:Error -> {error, Error}
   end.


% test
% kvets:update_cluster_metadata(#sys_meta{id = 1, status=closed, last_scn=100, nodes=[ra1@localhost, ra2@localhost, ra3@localhost, ra4@localhost]}).
update_cluster_metadata(Cluster) ->
	ets:insert(sysRam, Cluster),
	dets:insert(sysDisk, Cluster),
	ok.


%mode ram | disk | both
update_cluster_metadata(Cluster, Mode) ->
   case Mode of
      ram -> ets:insert(sysRam, Cluster);
     disk -> dets:insert(sysDisk, Cluster);
     both -> update_cluster_metadata(Cluster)
   end,
   {ok, Cluster}.



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


