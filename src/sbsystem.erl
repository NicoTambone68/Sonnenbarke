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
   next_scn/0,
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
   get_scn -> Response = next_scn();
   {set_cluster_status, Status} -> Response = new_cluster_status(Status);
   _ -> Response = no_data
end,
{reply, Response, state()}.

next_scn() ->
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
%  TO DO: Read default data from config/sys.config
%         and set NewCMeta accordingly
% something like this. (Note filter in this example is wrong) 
% {ok,K}=application:get_env(system_metadata, cluster).
% lists:filter(fun(E) -> {name,_} = E  end, K).
% 
create_cluster_metadata() ->
   % New Metadata Record w default values
   NewCMeta = ?SYSTEM{},
   sbdbs:open_tables(),
   sbdbs:update_cluster_metadata(NewCMeta),
   sbdbs:close_tables().


read_cluster_name() ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   CMeta?SYSTEM.name.

new_cluster_status(Status) ->
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

