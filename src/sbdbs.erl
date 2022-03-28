-module(sbdbs).
-include("sys_meta.hrl").

% TO DO: rename _table to _ts
-export([
	 open_tables/0, 
	 close_tables/0,
	 open_table/1,
	 close_table/1,
	 insert_table/2,
	 lookup_table/2,
	 match_ts/2,
	 match_delete_ts/2,
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
open_table(TSName) ->
   % build the following filename: .ra/<current_node>/sys_meta.dets	
   {ok, CDHomeDir} = sbenv:get_cluster_env(cluster_datafiles_home_dir),
   FName = atom_to_list(TSName) ++ atom_to_list('.dets'),
   FileName = string:join([CDHomeDir, atom_to_list(node()), FName], "/"),
   TsRam = ets:whereis(TSName),
   if TsRam == undefined ->
      ets:new(TSName, [ordered_set, public, named_table]);
      true -> ts_ram_already_open
   end,
   dets:open_file(TSName, [{file, FileName}, {type, set}]).


close_table(TSName) ->
   ets:delete(TSName),
   dets:close(TSName).


insert_table(TSName, Value) ->
   ?MODULE:open_table(TSName),
   EtsRet  = ets:insert(TSName, Value),
   DetsRet = dets:insert(TSName, Value),
   ?MODULE:close_table(TSName),
   {EtsRet, DetsRet}.


lookup_table(TSName, Pattern) ->
   ?MODULE:open_table(TSName),
   EtsRet  = dets:lookup(TSName, Pattern),
   % TO DO: load dets into ets at first lookup
   %DetsRet = ets:lookup(TSName, Value),
   ?MODULE:close_table(TSName),
   {ok, EtsRet}.

% return one or more tuples matching the pattern Pattern
match_ts(TSName, Pattern) ->
   ?MODULE:open_table(TSName),
   EtsRet  = dets:match_object(TSName, Pattern),
   ?MODULE:close_table(TSName),
   {ok, EtsRet}.

% return one or more tuples matching the pattern Pattern
% delete the matching pattern from the TS
match_delete_ts(TSName, Pattern) ->
   {_, Ret} = ?MODULE:match_ts(TSName, Pattern),
   ?MODULE:open_table(TSName),
   dets:match_delete(TSName, Pattern),
   ?MODULE:close_table(TSName),
   {ok, Ret}.

% Convert a custom pattern with wildcard 'any'
% to a pattern to be used with dets:match_object
%translate_pattern(Pattern) ->
%   List = tuple_to_list(Pattern),
%   ok.
   % TO DO: to be completed
   % e.g.
   % L = tuple_to_list({pippo, pluto, paperino}).
   % lists:map(fun(X) -> case X of paperino -> '_'; _ -> X end end, L).

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


