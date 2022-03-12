-module(kvets).
-include("sys_meta.hrl").


-export([
	 open_tables/0, 
	 close_tables/0,
         update_cluster_metadata/1,
         update_cluster_metadata/2,
	 get_cluster_metadata/1
	]).

% TO DO: read from config file
-define(RA_HOME_DIR, "./.ra").
% TO DO: replace cabled filename w define
-define(SYSTEM_TAB_FILENAME, "sys_meta.dets").


open_tables() ->
   % build the following filename: .ra/<current_node>/sys_meta.dets	
%   FileName = string:join([".ra", atom_to_list(node()), "sys_meta.dets"], "/"),
   FileName = string:join([?RA_HOME_DIR, atom_to_list(node()), "sys_meta.dets"], "/"),
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

close_tables() ->
   ets:delete(sysRam),
   ets:delete(sysIndex),
   dets:close(sysDisk).

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


