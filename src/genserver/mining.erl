-module(mining).
-export([
  proof/2
]).

proof(JsonStart, JsonEnd) ->
  {ok, FakeBlock} = application:get_env(master, genesis_block),
  {ok, FakeBlock}.
