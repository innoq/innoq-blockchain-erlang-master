%%%-------------------------------------------------------------------
%%% @author Christoph Iserlohn <ci@Christophs-MacBook-Pro-5.local>
%%% @copyright (C) 2018, Christoph Iserlohn
%%% @doc
%%%
%%% @end
%%% Created : 19 Apr 2018 by Christoph Iserlohn <ci@Christophs-MacBook-Pro-5.local>
%%%-------------------------------------------------------------------
-module(mining).

-behaviour(gen_server).

%% API
-export([start_link/0, proof/2, get_nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(mining_state, {origin, json_start, json_end, last_max, nodes}).


%%%===================================================================
%%% API
%%%===================================================================

proof(JsonStart, JsonEnd) ->
    gen_server:cast({global, mining}, {proof, self(), JsonStart, JsonEnd}),
    io:format("receiving...\n", []),
    io:format("PID: ~p\n", [self()]),
    receive
		{ok, Block, Sha256} ->
			io:format("Received: ~p, ~p\n", [Block, Sha256]),
			{ok, Block, Sha256};
		{error, Message} ->
			{error, Message};
		Any ->
			io:format("Unknown message: ~p", [Any]),
			{error, "Unknown message!"} %TODO: insert Any into error message!
	after 15000 ->
		{error, "Timeout!"}
	end.

get_nodes() ->
	{ok, Nodes} = gen_server:call({global, mining}, {get_nodes}),
	Nodes.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #mining_state{nodes = [], origin = false}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_call({get_nodes}, _From, State) ->
    {reply, {ok, State#mining_state.nodes}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.

handle_cast({node_up, NewNode = {Name, _LoadFactor}}, State) ->
    io:format("Node ~p is up.\n", [Name]),
    {noreply, State#mining_state{nodes = State#mining_state.nodes ++ [NewNode]}};
handle_cast({node_down, {Name}}, State) ->
    io:format("Node ~p is down.\n", [Name]),
    {noreply, State#mining_state{nodes = lists:keydelete(Name, 1, State#mining_state.nodes)}};
handle_cast({proof, Origin, JsonStart, JsonEnd}, State) ->
        io:format("proof called.\n", []),
	case State#mining_state.origin of
		false ->
			InitState = init_mining_state(Origin, JsonStart, JsonEnd, State),
			NewState = lists:foldl(fun instruct_node/2, InitState, State#mining_state.nodes),
			{noreply, NewState};
		_Any ->
			Origin ! {error, "Mining busy!"},
			{noreply, State}
	end;
handle_cast({proof_found, _Name, Block, Sha256}, State) ->
    io:format("proof_found called\n", []),
	case State#mining_state.origin of
	  false -> % mining already done...
    	{noreply, State};
      _Any ->
		State#mining_state.origin ! {ok, Block, Sha256},
    	{noreply, State#mining_state{origin = false}}
	end;
handle_cast({no_proof_found, Name, _Message}, State) ->
    io:format("nod_proof_found called.\n", []),
	case State#mining_state.origin of
	  false -> % mining already done...
    	{noreply, State};
      _Any ->
	  	Node = lists:keyfind(Name, 1, State#mining_state.nodes),
    	{noreply, instruct_node(Node, State)}
	end;
handle_cast(Request, State) ->
    io:format("Wrong Cast: Got: ~p\n", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
	Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_mining_state(Origin, JsonStart, JsonEnd, State) ->
	State#mining_state{json_start = JsonStart, json_end = JsonEnd, last_max = 0, origin = Origin}.

instruct_node({Name, LoadFactor}, State) ->
    io:format("Calling Node: ~p, LoadFactor: ~p\n", [Name, LoadFactor]),
	To = State#mining_state.last_max + (1000000 * LoadFactor),
	gen_server:cast({global, Name}, {mine, State#mining_state.json_start, State#mining_state.json_end, State#mining_state.last_max + 1, To, 6}),
	State#mining_state{last_max = To}.
