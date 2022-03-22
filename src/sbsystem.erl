%% This module is an interface to system metadata during runtime
%% Its purpose is to provide efficiently cluster state informations at runtime
%% To be launched by a supervisor so when the process stops for no data, it will be recovered
-module(sbsystem).
-behaviour(gen_server).
-include("sys_meta.hrl").

-export([
   start/0,
   stop/0,
   init/1,
   get_cluster_metadata/0,
   get_cluster_metadata/1,
   set_cluster_status/1,
   get_cluster_name/0,
   create_cluster_metadata/0,
   update_cluster_metadata/1,
   get_scn/0,
%   _get_scn/0,
   am_i_leader/0,
   handle_call/3,
   handle_cast/2
]).

% index of the SCN counter sequence
-define(SCN, 1).
-define(SYSTEM, #sys_meta).

start() ->
  %% start the named gen server
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
   sbdbs:open_tables(),
   {Response, Cluster} = sbdbs:get_cluster_metadata(disk),
   if Response == ok ->
        {NewResp, _} = sbdbs:update_cluster_metadata(Cluster, ram), 
         % SCN sequence starts from last_scn incremented by one
         init_scn(Cluster?SYSTEM.last_scn+1);
      Response == error -> NewResp = ok 
   end,
   {NewResp, Cluster}.

% gentle stop, terminate and close table
% see handle_cast
stop() ->
   gen_server:cast(?MODULE, stop).	

state() -> 0.

% Initialize SCN to the last SCN of the cluster metadata
init_scn(Scn) ->
    Cargs = [1, {1, Scn}],
    sbcount:start(Cargs).

get_cluster_metadata() ->
   Cluster = get_cluster_metadata(ram),
   {ok, Cluster}.

get_cluster_name() ->
   gen_server:call(?MODULE, get_cluster_name).

% From = disk | ram
get_cluster_metadata(From) ->
   gen_server:call(?MODULE, {get_cluster_metadata, From}).

get_scn() ->
   gen_server:call(?MODULE, get_scn).

% status: closed | restricted | open
set_cluster_status(Status) ->
   gen_server:call(?MODULE, {set_cluster_status, Status}).

handle_call(Call, _From, _ ) -> 
case Call of	
   {get_cluster_metadata, From} ->	
   case From of
      ram -> {_, Response} = sbdbs:get_cluster_metadata(ram);
     disk -> {_, Response} = sbdbs:get_cluster_metadata(disk)
   end;
   get_cluster_name -> Response = read_cluster_name();
   get_scn -> Response = get_scn_();
   {set_cluster_status, Status} -> Response = set_cluster_status_(Status);
   _ -> Response = no_data
end,
{reply, Response, state()}.

get_scn_() ->
   Scn = sbcount:get_sequence(?SCN),
   ?MODULE:update_cluster_metadata(Scn),
   Scn.


% commit changes to disk
% this is called whithin a transaction
% and executed on all active nodes
%  TO DO: better change this name to update_node_metadata
update_cluster_metadata(Scn) ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   CMetaUpdated = CMeta?SYSTEM{last_scn = Scn},
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   {ok, CMetaUpdated}.


% Create a new metadata storage 
%  WARNING! Overwrites data
% TO DO: check and debug
% 
create_cluster_metadata() ->
   % Read initial values from config/sys.config
   {ok,L} = application:get_env(system_metadata, cluster),
   [{name, ClusterName}] = lists:filter(fun({X,_}) -> X == name end, L),
   [{starting_scn, StartingScn}] = lists:filter(fun({X,_}) -> X == starting_scn end, L),
   [{ra_home_dir, RaHomeDir}] = lists:filter(fun({X,_}) -> X == ra_home_dir end, L),
   [{cluster_datafiles_home_dir, CDHomeDir}] = lists:filter(fun({X,_}) -> X == cluster_datafiles_home_dir end, L),
   [{nodes, Nodes}] = lists:filter(fun({X,_}) -> X == nodes end, L),
   % If it doesn't exist create the home directory for cluster datafiles and metadata
   % The path structure is: cluster_datafiles_home_dir/node_name
   CDHomeDirNode = lists:concat([CDHomeDir, "/", node()]),
   case filelib:is_dir(CDHomeDirNode) of
	   false -> file:make_dir(CDHomeDirNode);
	   true -> ok
   end,
   % Same story for the .ra home directory
   RaHomeDirNode = lists:concat([RaHomeDir, "/", node()]),
   case filelib:is_dir(RaHomeDirNode) of
	   false -> file:make_dir(RaHomeDirNode);
	   true -> ok
   end,
   % New Metadata Record w default values
   NewCMeta = ?SYSTEM{ name = ClusterName
		      ,last_scn = StartingScn
		      ,nodes = Nodes},
   sbdbs:open_tables(),
   sbdbs:update_cluster_metadata(NewCMeta),
   sbdbs:close_tables().


read_cluster_name() ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   CMeta?SYSTEM.name.

set_cluster_status_(Status) ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   CMetaUpdated = CMeta?SYSTEM{status = Status},
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   ok.

handle_cast(stop, State) ->
   sbdbs:close_tables(),
   {stop, normal, State}.

am_i_leader() ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   {_, Leader} = ra_leaderboard:lookup_leader(CMeta?SYSTEM.name),
   {Leader == node(), Leader}.

