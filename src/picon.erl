%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(picon).

-export([connect/1,
	 connect/2,
	 modify/2,
	 del/1,
	 get_status/0,
	 get_status/1]).

-include("picon.hrl").

-define(SERVER, picon_server).

%% @doc connect nodes
%% @end
-spec connect(term()) -> [#connection{}] | #connection{}.
connect(local) ->
	gen_server:call(?SERVER, {connect, local});

connect(listed) ->
	gen_server:call(?SERVER, {connect, application:get_env(?APPLICATION, listed), permanent});

connect(NodeS) ->
	gen_server:call(?SERVER, {connect, NodeS, permanent}).

connect(NodeS, CType) ->
	gen_server:call(?SERVER, {connect, NodeS, CType}).

%% @doc modify a connection
%% @end
-spec modify(node(), permanent | temporary) -> #connection{}.
modify(Node, CType) ->
	gen_server:call(?SERVER, {modify, Node, CType}).

%% @doc delets/removes a node from cluster
%% @end
-spec del(node()) -> #connection{}.
del(Node) ->
	gen_server:call(?SERVER, {remove, Node}).

%% @doc gets the status of all nodes in cluster
%% @end
-spec get_status() -> [#connection{}].
get_status() ->
	gen_server:call(?SERVER, {get, status, any}).

%% @doc gets the status of a specified node
%% @end
-spec get_status(node()) -> #connection{}.
get_status(Node) ->
	gen_server:call(?SERVER, {get, status, Node}).

