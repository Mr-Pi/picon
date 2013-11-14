%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(picon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	lager:debug("start link"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
-spec init(term()) -> {ok, {term, [term()]}} |
                       ignore |
                       {error, term()}.
init([]) ->
	lager:debug("init: Opts='[]'"),
	Backup = ?CHILD(picon_backup, picon_backup, worker, []),
	Server = ?CHILD(picon_server, picon_server, worker, []),
	{ok, {{one_for_one, 5, 10}, [Backup, Server]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


