-module(master_sup).
-behaviour(supervisor).

%% External exports
-export([
  start_link/0
]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [master_config:web_config()]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    State = {state,
           {state, start_link, []},
           permanent, 5000, worker, [state]},
    Mining = {mining,
           {mining, start_link, []},
           permanent, 5000, worker, [mining]},

    Processes = [Web, State, Mining],
    {ok, { {one_for_one, 10, 10}, Processes} }.
