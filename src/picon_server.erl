%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(picon_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% API
-export([start_link/0,
	 add/1,
	 del/1,
	 connect_local/0,
	 connect_listed/0]).

-include("picon.hrl").

-record(state, {trials=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	lager:debug("start link"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc adds a node to cluster
%% @end
-spec add(node()) -> #connection{}.
add(Node) ->
	lager:debug("add node ~p", [Node]),
	?NYI_T.

%% @doc removes a node from cluster
%% @end
-spec del(node()) -> #connection{}.
del(Node) ->
	lager:debug("removes node ~p", [Node]),
	?NYI_T.

%% @doc connect all local nodes
%% @end
-spec connect_local() -> [#connection{}].
connect_local() ->
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
	[add(Node) || Node <- Nodes].
	

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	lager:debug("init: Opts='[]'"),
	lists:foreach(
		fun(Type) ->
			case Type of
				local -> picon:connect_local_nodes();
				listed -> ?NYI_T
			end
		end,
		application:get_env(?APPLICATION, startup_connection, [])
		),
	picon_monitor:add_handler(picon_monitor),
	net_kernel:monitor_nodes(true),
	State = #state{},
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
	lager:warning("unexpected call: Request='~p', From='~p', State='~p'", [Request, From, lager:pr(State,?MODULE)]),
	Reply = unexpected,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
	lager:warning("unexpected cast: Msg='~p', State='~p'", [Msg, lager:pr(State,?MODULE)]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	lager:warning("unexpected info: Info='~p', State='~p'", [Info, lager:pr(State,?MODULE)]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	lager:debug("terminate: Reason='~p', State='~p'", [Reason, lager:pr(State,?MODULE)]),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	lager:notice("code change: OldVsn='~p', State='~p', Extra='~p'", [OldVsn, lager:pr(State,?MODULE), Extra]),
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc connect node
%% @end
-spec connect_node(node(), reference(), non_neg_integer(), non_neg_integer(), waiting | reconnecting) -> none().
connect_node(Node, Ref, _ReconnectTime, 0, _CType) ->
	lager:debug("try to connect node ~p", [Node]),
	case net_adm:ping(Node) of
		pong ->
			picon_monitor:connected(Node, Ref),
			lager:info("successfully connected node ~p", [Node]);
		pang ->
			picon_monitor:failed(Node, Ref),
			lager:info("fails to connect node ~p", [Node])
	end;
connect_node(Node, Ref, ReconnectTime, Count, CType) ->
	lager:debug("try to connect node ~p", [Node]),
	case net_adm:ping(Node) of
		pong ->
			picon_monitor:connected(Node, Ref),
			lager:info("successfully connected node ~p", [Node]);
		pang ->
			receive
				{get_status, Ref, From} ->
					From ! #connection{state=CType, node=Node, retrials=Count-1}
			after
				0 ->
					ok
			end,
			timer:sleep(ReconnectTime),
			lager:debug("~p attempts remaining to connect ~p", [Count-1, Node]),
			connect_node(Node, Ref, ReconnectTime, Count-1, CType)
	end.

