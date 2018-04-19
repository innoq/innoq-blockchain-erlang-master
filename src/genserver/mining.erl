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
-export([start_link/0, proof/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

proof(JsonStart, JsonEnd) ->
    gen_server:cast({global, ?SERVER}, {proof, self(), JsonStart, JsonEnd}),
    io:format("receiving...\n", []),
    io:format("PID: ~p\n", [self()]),
    {RBlock, RSha256} = receive
			    {true, Block, Sha256} ->
				io:format("Received: ~p, ~p\n", [Block, Sha256]),
				{Block, Sha256};
			    Any ->
				io:format("~p", [Any])
			after 15000 ->
				{"", ""}
			end,
    %{ok, FakeBlock} = application:get_env(master, genesis_block),
    %{ok, FakeBlockSha256} = application:get_env(master, genesis_block_sha256),
    {ok, RBlock, bin_to_hexstr(RSha256)}.


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
    {ok, #{}}.

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

handle_cast({node_up, Name}, State) ->
    io:format("Node ~p is up.\n", [Name]),
    {noreply, State};
handle_cast({node_down}, State) ->    
    {noreply, State};
handle_cast({proof_done, Origin, Block, Sha256}, State) ->
    Origin ! {ok, Block, Sha256},
    {noreply, State};
handle_cast({proof, Origin, JsonStart, JsonEnd}, State) ->
    %{ok, FakeBlock} = application:get_env(master, genesis_block),
    %{ok, FakeBlockSha256} = application:get_env(master, genesis_block_sha256),
    gen_server:cast({global, slave_server}, {mine, Origin, JsonStart, JsonEnd, 0, 1000000, 2}),
    {noreply, State};
handle_cast(_Request, State) ->
    io:format("Wrong Cast\n", []),
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

bin_to_hexstr(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)])).
