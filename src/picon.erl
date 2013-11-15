%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%% == Callback module for picon ==
%%%   Users should only use these functions
%%%
%%% @TODO NOTHING IMPLEMENTED YET
%%% @end
%%%-------------------------------------------------------------------
-module(picon).

-export([]).

-include("picon.hrl").
-define(SERVER, picon_server).

%%%===================================================================
%%% Type definitions
%%%===================================================================

-type nodes() :: [node()].
%% defines a list of erlang nodes

-type node_list() :: atom().
%% reverence to a list of nodes, taken form configuration

-type connection_type() :: temporary | permanent | local.
%% permanent nodes automatically reconnect, temporary doesn't

-type connection() :: #connection{}.
%% parameters of a connected nodes
%% ```
%% #connection{
%%   state :: connected | disconnected | waiting | reconnecting | removed | timeout | undefined,
%%   type :: picon:connection_type(),
%%   reconnects :: non_neg_integer(),
%%   retrials :: non_neg_integer()
%% }.
%% '''
%%
%% the first element of the tuple retrials is a counter of success reconnects,
%% the second element is returns the number of failed recovery attempts

-export_type([nodes/0, node_list/0, connection_type/0, connection/0]).


%%%===================================================================
%%% API
%%%===================================================================

-export([connect/2,
	 remove/1,
	 modify/2,
	 getStatus/1
	]).

%% @doc connect specific node or a list of nodes
%% @end
-spec connect(node() | nodes() | node_list() | local, connection_type()) ->
	[{node(), connection()}] | connection().
connect(NodeS, CType) ->
	gen_server:call(?SERVER, {connect, NodeS, CType}).

%% @doc removes node(s) from cluster
%% @end
-spec remove(node() | nodes() | node_list() | local) -> ok.
remove(NodeS) ->
	gen_server:call(?SERVER, {remove, NodeS}).

%% @doc modify the connection_type of node(s)
%% @end
-spec modify(node() | nodes() | node_list(), connection_type()) -> ok.
modify(NodeS, CType) ->
	gen_server:call(?SERVER, {modify, NodeS, CType}).

%% @doc gets the status of connected nodes or a specific node(s)
-spec getStatus(node() | nodes() | node_list() | local | any) -> 
	[{node(), connection()}] | connection().
getStatus(NodeS) ->
	gen_server:call(?SERVER, {get, status, NodeS}).

