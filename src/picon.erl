%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(picon).

-export([connect_local/0,
	 connect_listed/0,
	 connect_listed/1,
	 add/1,
	 add/2,
	 modify/2,
	 del/1,
	 get_status/0,
	 get_status/1]).

-include("picon.hrl").

%% @doc connect all local nodes
%% @end
-spec connect_local() -> [#connection{}].
connect_local() ->
	picon_server:connect_local().

%% @doc adds all listed nodes to cluster
%% @end
-spec connect_listed() -> [#connection{}].
connect_listed() ->
	?NYI_T.

%% @doc adds all nodes in list to cluster, will expand to connect_listed(nodes(), permanent)
%% @end
-spec connect_listed(nodes()) -> [#connection{}].
connect_listed(Nodes) ->
	connect_listed(Nodes, permanent).

%% @doc adds all nodes in list to cluster, temporary or permanent
%% @end
-spec connect_listed(nodes(), permanent | temporary) -> [#connection{}].
connect_listed(Nodes, CType) ->
	?NYI_T.

%% @doc adds a node to cluster, will expand to add(node(), permanent)
%% @end
-spec add(node()) -> #connection{}.
add(Node) ->
	add(Node, permanent).

%% @doc adds a node to cluster
%% @end
-spec add(node(), permanent | temporary) -> #connection{}.
add(Node, CType) ->
	?NYI_T.
	
%% @doc modify a connection
%% @end
-spec modify(node(), permanent | temporary) -> #connection{}.
modify(Node, CType) ->
	?NYI_T.

%% @doc delets/removes a node from cluster
%% @end
-spec del(node()) -> #connection{}.
del(Node) ->
	?NYI_T.

%% @doc gets the status of all nodes in cluster
%% @end
-spec get_status() -> [#connection{}].
get_status() ->
	?NYI_T.

%% @doc gets the status of a specified node
%% @end
-spec get_status(node()) -> #connection{}.
get_status(Node) ->
	?NYI_T.

