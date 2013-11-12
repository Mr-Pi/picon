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
-export([start_link/0]).

-include("picon.hrl").

-record(state, {synced=false, connecting=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc connect all local nodes, returns a list of all connected local nodes
%% @end
-spec connect_local() -> nodes().
connect_local() ->
	lager:debug("connecting local nodes..."),
	{ok, NodesPrefixIn} = net_adm:names(),
	Domains = [ lists:last(string:tokens(atom_to_list(node()),[$@])) | [net_adm:localhost()] ],
	NodesPrefix = lists:map(fun(X) -> {NodePrefix,_}=X, NodePrefix end, NodesPrefixIn),
	Nodes = [ list_to_atom(NodePrefix ++ "@" ++ Domain) ||
		  NodePrefix <- NodesPrefix, Domain <- Domains ],
	[ Node || Node <- Nodes, net_adm:ping(Node) =:= pong ].

%% @doc Starts the server
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	lager:debug("start link"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


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
	State = #state{
		connecting = lists:concat([
			case StartUp of
				local ->
					lists:foreach(fun(Node) ->
						put(Node, #connection{node=Node, state=connected, type=temporary})
					end, connect_local()),
					[];
				listed ->
					connect(application:get_env(?APPLICATION, listed, []), permanent)
			end ||
			StartUp <- application:get_env(?APPLICATION, startup_connection, [])]
			)
		},
	picon_monitor:add_handler(picon_monitor),
	net_kernel:monitor_nodes(true),
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
handle_call({get, status, any}, _From, State) ->
	Reply = [ Connection || {_Node, Connection} <- get(), is_record(Connection, connection) ],
	{reply, Reply, State};
handle_call({get, status, Node}, _From, State) ->
	{reply, get(Node), State};
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
handle_cast({retry, Node, Count}, State) ->
	Connection = get(Node),
	{Retrials, _} = Connection#connection.retrials,
	put(Node, Connection#connection{retrials={Retrials, Count}}),
	{noreply, State};
handle_cast({failed, Node}, State) ->
	Connection = get(Node),
	{Retrials, _} = Connection#connection.retrials,
	put(Node, Connection#connection{retrials={Retrials+1, 0}, state=timeout}),
	NewState = State#state{connecting = proplists:delete(Node, State#state.connecting)},
	{noreply, NewState};
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
handle_info({nodedown, Node}, State) ->
	?NYI_T;
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


%% @doc connects a list of nodes, returns the pids of connecting processes
%% @end
-spec connect(nodes(), temporary | permanent) -> [{node(),pid()}].
connect(Nodes, CType) when is_list(Nodes) ->
	lager:debug("tries to connect remote nodes ~p", [Nodes]),
	[
	 {Node, erlang:spawn_link(
		  fun() -> connect_node(Node, 0) end)} ||
	 Node <- Nodes,
	 put(Node, #connection{node=Node, state=waiting, type=CType}) =/= xyz %the result must always true
	 ].

%% @private
%% @doc connect node
%% @end
-spec connect_node(node(), non_neg_integer()) -> none().
connect_node(Node, Count) ->
	lager:debug("try to connect node ~p", [Node]),
	case net_adm:ping(Node) of
		pong ->
			gen_server:cast(?MODULE, {success, Node, Count}),
			lager:info("successfully connected node ~p", [Node]);
		pang ->
			case Count >= application:get_env(?APPLICATION, reconnect_trials, 5) of
				true ->
					gen_server:cast(?MODULE, {failed, Node}),
					lager:info("fails to connect node ~p", [Node]);
				false ->
					gen_server:cast(?MODULE, {retry, Node, Count}),
					ReconnectTime = application:get_env(?APPLICATION, reconnect_sleep, 5000),
					timer:sleep(ReconnectTime),
					connect_node(Node, Count+1)
			end
	end.

