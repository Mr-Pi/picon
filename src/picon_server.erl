%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(picon_server).

-behaviour(gen_server).

-include("picon.hrl").
-include("defaults.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {synced=false, backup=true}).
-record(node_table, {node, vsn=0, state, type, reconnects=0, retrials=0}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
start_link() ->
	lager:debug("start link"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
init([]) ->
	lager:debug("init: Opts='[]'"),
	case picon_backup:get_node_table() of
		true ->
			receive
				{'ETS-TRANSFER',picon_nodes,_,_} -> lager:info("node table restored")
			after
				?TIMEOUT -> lager:alert("failed to restore node table")
			end;
		false ->
			lager:debug("create node table"),
			ets:new(picon_nodes, [set, public, named_table, compressed,
					      {keypos, #node_table.node},
					      {heir, erlang:whereis(picon_backup), []}])
	end,
	monitor(process, picon_backup),
	State = case nodes() of
			[] ->
				#state{synced=true};
			Nodes ->
				sync(Nodes)
		end,
	net_kernel:monitor_nodes(true), % TODO: set correct flags
	lager:debug("adds local node to node table"),
	add_to_table(node(), 0, connected, application:get_env(picon, self_type, ?PI_self_type), 0, 0),
	{ok, State}.

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
handle_call({connect, NodeS, CType}, _From, State) ->
	Reply = connect(NodeS, CType),
	{reply, Reply, State};

handle_call(Request, From, State) ->
	lager:warning("unexpected call: Request='~p', From='~p', State='~p'", [Request, From, lager:pr(State,?MODULE)]),
	Reply = ok,
	{reply, Reply, State}.

%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
handle_cast(Msg, State) ->
	lager:warning("unexpected cast: Msg='~p', State='~p'", [Msg, lager:pr(State,?MODULE)]),
	{noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
handle_info({'DOWN', _Ref, process, {picon_backup, Node}, _Reason}, State) when Node =:= node() ->
	lager:warning("lost backup process"),
	{noreply, State#state{backup=false}};

handle_info({backup, up, Pid}, State) ->
	ets:setopts(picon_nodes, [{heir, Pid, []}]),
	lager:info("receive new backup process ~p", [Pid]),
	{noreply, State#state{backup=true}};

handle_info(Info, State) ->
	lager:warning("unexpected info: Info='~p', State='~p'", [Info, lager:pr(State,?MODULE)]),
	{noreply, State}.

%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
terminate(Reason, State) ->
	lager:debug("terminate: Reason='~p', State='~p'", [Reason, lager:pr(State,?MODULE)]),
	ok.

%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
code_change(OldVsn, State, Extra) ->
	lager:notice("code change: OldVsn='~p', State='~p', Extra='~p'", [OldVsn, lager:pr(State,?MODULE), Extra]),
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc calls the correct connection function
%% works mostly as a bridge
%% @end
-spec connect(node() | picon:nodes() | picon:node_list() | local, picon:connection_type()) ->
	[{node(), picon:connection()}] | picon:connection().
connect(NodeS, CType) when is_atom(NodeS) ->
	lager:debug("node(s) ~p should be connects as ~p", [NodeS, CType]),
	case proplists:is_defined(NodeS, application:get_env(picon, lists, ?PI_lists)) of
		false ->	% NodeS is a single Node
			lager:debug("~p is the name of a single node"),
			connect_node(NodeS, CType);
		true ->		% NodeS is a list of nodes, defined in configuration
			lager:debug("~p is the name of a node_list"),
			?NYI
	end.


%% @doc connects a node
%% @end
-spec connect_node(node(), picon:connection_type()) -> connected | waiting | reconnecting.
connect_node(Node, CType) ->
	lager:debug("node ~p should be connected ~p", [Node, CType]),
	case ets:member(picon_nodes, Node) of
		false ->
			CState =
			case net_adm:ping(Node) of
				pong ->
					lager:debug("node ~p has respond", [Node]),
					connected;
				pang ->
					erlang:spawn_link(
					  fun() ->
						timer:sleep(application:get_env(picon, retrials_interval, ?PI_retrials_interval)),
						connect_node(Node, CType)
					  end)
			end,
			add_to_table(Node, 0, CState, CType, 0, 0);
		true ->
			?NYI %means reconnecting
	end.

%% @doc sync nodes
%% @end
-spec sync(picon:nodes()) -> ok.
sync(Nodes) ->
	lager:info("synchronize nodes ~p", [Nodes]),
	?NYI.


%% @doc updates node table
%% @end
-spec add_to_table(node(), non_neg_integer(),
		   connected | disconnected | waiting | reconnecting | removed | timeout | undefined,
		   picon:connection_type(), non_neg_integer(), non_neg_integer()) -> updated | insert | same | old.
add_to_table(Node, Vsn, State, CType, Reconnects, Retrials) ->
	ExistVsn = case ets:member(picon_nodes, Node) of
			   true -> ets:lookup_element(picon_nodes, Node, #node_table.vsn);
			   false -> -1
		   end,
	case Vsn of
		ExistVsn ->
			same;
		NewVsn when ExistVsn < Vsn ->
			ets:insert(picon_nodes,
				   #node_table{node=Node, vsn=NewVsn, state=State, type=CType,
					       reconnects=Reconnects, retrials=Retrials});
		_OldVsn ->
			old
	end.
	
