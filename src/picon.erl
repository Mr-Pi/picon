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
	 add/1]).

-include("picon.hrl").

%% @doc connects to all local nodes
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
-spec connect_listed() -> [{node(),reference()|already_connected}].
connect_listed() ->
	Nodes = application:get_env(picon,listed,[]),
	lager:debug("connect listed nodes: ~p", [Nodes]),
	[{Node,picon:add(Node)} || Node <- Nodes].


%% @doc add
%% @end
-spec add(node()) -> already_connected | reference().
add(Node) ->
	lager:debug("add node ~p", [Node]),
	case lists:member(Node,nodes()) of
		false -> picon_server:add(Node);
		true -> already_connected
	end.

