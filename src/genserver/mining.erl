-module(mining).
-export([
  proof/2
]).

proof(JsonStart, JsonEnd) ->
  {ok, FakeBlock} = application:get_env(master, genesis_block),
  {ok, FakeBlockSha256} = application:get_env(master, genesis_block_sha256),
  {ok, FakeBlock, FakeBlockSha256}.
