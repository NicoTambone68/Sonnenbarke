%%
%%
%%
%%
%% @doc This module is an interface to handle cluster metadata during runtime
%% Its purpose is to provide efficiently cluster state informations

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
   update_cluster_metadata/2,
   get_scn/0,
   am_i_leader/0,
   handle_call/3,
   handle_cast/2
]).

% index of the SCN counter sequence
-define(SCN, 1).
-define(SYSTEM, #sys_meta).

%% @doc Start the gen_server
%%
%% @param none
%%
%% @returns term()
%%
%% @end
-spec start() -> term().
start() ->
  %% start the named gen server
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initializes the module
%% 
%% @param _Args = term()
%%
%% @returns term()
%%
%% @end
-spec init(term()) -> term().
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
%% @doc Gently stops the gen_server, terminate and close the open tables
%% See handle_ for more details
%%
%% @param none
%%
%% @returns term()
%%
%% @end
-spec stop() -> term().
stop() ->
   gen_server:cast(?MODULE, stop).	

state() -> 0.

%% @doc Initializes SCN to the last SCN of the cluster metadata
%%
%% @param Scn = integer()
%%
%% @returns term().
%%
%% @end
-spec init_scn(integer()) -> term().
init_scn(Scn) ->
    Cargs = [1, {1, Scn}],
    sbcount:start(Cargs).

%% @doc Reads metadata from Ram
%%
%% @param none
%%
%% @returns term()
%%
%% @end
-spec get_cluster_metadata() -> term().
get_cluster_metadata() ->
   Cluster = get_cluster_metadata(ram),
   {ok, Cluster}.

%% @doc Returns the name of the cluster
%%
%% @param none
%%
%% @returns term()
%%
%% @end
-spec get_cluster_name() -> term().
get_cluster_name() ->
   gen_server:call(?MODULE, get_cluster_name).

%% @doc Reads metadata from either disk or ram
%%
%% @param From = atom() = disk | ram
%%
%% @returns term()
%%
%% @end
-spec get_cluster_metadata(atom()) -> term().
get_cluster_metadata(From) ->
   gen_server:call(?MODULE, {get_cluster_metadata, From}).

%% @doc Gets the current Scn
%%
%% @param none 
%%
%% @returns integer()
%%
%% @end
-spec get_scn() -> integer().
get_scn() ->
   gen_server:call(?MODULE, get_scn).

% status: closed | restricted | open

%% @doc Sets the current status of the cluster
%% currently closed | open
%% Other states will be added in further developments
%%
%% @param Status = atom() = closed | open
%%
%% @returns term()
%%
%% @end
-spec set_cluster_status(atom()) -> term().
set_cluster_status(Status) ->
   gen_server:call(?MODULE, {set_cluster_status, Status}).


%% @doc Callback function of the gen_server
%% Implements the calls set above
%%
%% @param Call, _From, _
%%
%% @returns term()
%%
%% @end
-spec handle_call(term(), term(), term()) -> term().
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

%% @doc Returns the next Scn number. Updates the cluster's metadata with the new Scn
%%
%% @param none
%%
%% @returns Scn = integer()
%%
%% @end
-spec get_scn_() -> integer().
get_scn_() ->
   Scn = sbcount:get_sequence(?SCN),
   ?MODULE:update_cluster_metadata(Scn),
   Scn.

%% @doc Updates the metadata on the current node
%% writing the given Scn. The other metadata information remains unaffected
%%
%% @param Scn = integer() 
%%
%% @returns {ok, Metadata = term()}
%%
%% @end
-spec update_cluster_metadata(integer()) -> term().
update_cluster_metadata(Scn) ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   CMetaUpdated = CMeta?SYSTEM{last_scn = Scn},
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   {ok, CMetaUpdated}.

%% @doc Updates metadata Tuple Space field with the list Nodes associated with the Tuple Space TS
%%
%% @param TSName = string, Nodes = [atom()]
%%
%% @returns term()
%%
%% @end
-spec update_cluster_metadata(string(), [atom()]) -> term().
update_cluster_metadata(TSName, Nodes) ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   CMetaUpdated = CMeta?SYSTEM{ts = {TSName,Nodes}},
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   {ok, CMetaUpdated}.

%% @doc Create a new default metadata set. WARNING! Overwrites existing data 
%%
%% @param none 
%%
%% @returns term()
%%
%% @end
-spec create_cluster_metadata() -> term().
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
	   % Note that with file library missing parent directories are not created
	   false -> file:make_dir(CDHomeDir),
		    file:make_dir(CDHomeDirNode);

	   true -> ok
   end,
   % Same story for the .ra home directory
   RaHomeDirNode = lists:concat([RaHomeDir, "/", node()]),
   case filelib:is_dir(RaHomeDirNode) of
	  false -> file:make_dir(RaHomeDir), 
		   file:make_dir(RaHomeDirNode);

	   true -> ok
   end,
   % New Metadata Record w default values
   NewCMeta = ?SYSTEM{ name = ClusterName
		      ,last_scn = StartingScn
		      ,nodes = Nodes},
   sbdbs:open_tables(),
   sbdbs:update_cluster_metadata(NewCMeta),
   sbdbs:close_tables().

%% @doc Gets the cluster name
%%
%% @param none
%%
%% @returns atom()
%%
%% @end
-spec read_cluster_name() -> term().
read_cluster_name() ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   CMeta?SYSTEM.name.

%% @doc Set the current cluster status (open|close)
%%
%% @param Status = string()
%%
%% @returns atom()
%%
%% @end
-spec set_cluster_status_(atom()) -> atom().
set_cluster_status_(Status) ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   CMetaUpdated = CMeta?SYSTEM{status = Status},
   sbdbs:update_cluster_metadata(CMetaUpdated, both),
   ok.

%% @doc Callback function for gen_server stopping
%%
%% @param stop = atom(), State = term()
%%
%% @returns term()
%%
%% @end
-spec handle_cast(atom(), term()) -> term().
handle_cast(stop, State) ->
   sbdbs:close_tables(),
   {stop, normal, State}.

%% @doc Returns true if the current node is leader, otherwise returns false
%%
%% @param none
%%
%% @returns atom()
%%
%% @end
-spec am_i_leader() -> boolean().
am_i_leader() ->
   {_, CMeta} = sbdbs:get_cluster_metadata(ram),
   {_, Leader} = ra_leaderboard:lookup_leader(CMeta?SYSTEM.name),
   {Leader == node(), Leader}.


