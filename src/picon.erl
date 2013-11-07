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
	 add/1,
	 del/1]).

-include("picon.hrl").

%% @doc connect all local nodes
%% @end
-spec connect_local() -> [#connection{}].
connect_local() ->
	picon_server:connect_local().

%% @doc connect all listed nodes
%% @end
-spec connect_listed() -> [#connection{}].
connect_listed() ->
	picon_server:connect_listed().

%% @doc adds a node to cluster
%% @end
-spec add(node()) -> #connection{}.
add(Node) ->
	picon_server:add(Node).


%% @doc removes a node from cluster
%% @end
-spec del(node()) -> #connection{}.
del(Node) ->
	picon_server:del(Node).

