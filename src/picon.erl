%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(picon).

-export([connect_local_nodes/0,
	 connect_listed/0,
	 add/1,
	 del/1]).

-include("picon.hrl").

%% @doc connect all local nodes
%% @end
-spec connect_local_nodes() -> nodes().
connect_local_nodes() ->
	lager:debug("connect all local nodes"),
	{ok, NodesPrefixIn} = net_adm:names(),
	Domains = [ lists:last(string:tokens(atom_to_list(node()),[$@])) | [net_adm:localhost()] ],
	NodesPrefix = lists:map(fun(X) -> {NodePrefix,_}=X, NodePrefix end, NodesPrefixIn),
	Nodes = [ list_to_atom(NodePrefix ++ "@" ++ Domain) || NodePrefix <- NodesPrefix, Domain <- Domains ],
	net_adm:ping_list(Nodes),
	nodes().


%% @doc connect all listed nodes
%% @end
-spec connect_listed() -> [#connection{}].
connect_listed() ->
	Nodes = application:get_env(?APPLICATION, listed, []),
	lager:debug("connect listed nodes: ~p", [Nodes]),
	[{Node,picon:add(Node)} || Node <- Nodes].


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

