-module(block_utils).

-export ([
  first_chain_element/0,
  build_new_block/0,
  build_new_block_data/0
]).

%%--------------------------------------------------------------------
%% @doc
%% Generates the first chain element
%% @end
%% -------------------------------------------------------------------
first_chain_element() ->
  {ok, GenesisBlockJson} = application:get_env(master, genesis_block),
  {ok, GenesisBlockSha256} = application:get_env(master, genesis_block_sha256),
  #{json => GenesisBlockJson, sha256 => GenesisBlockSha256}.

build_new_block() ->
  {ok, Transactions} = state:fetch_transactions(),
  {ok, LastElement} = state:get_last_chain_element(),
  {ok, NextIndex} = state:get_next_index(),
  LastBlockSha256 = maps:get(sha256, LastElement),
  #{<<"index">> => NextIndex,
    <<"timestamp">> => unixtime:gettime(),
    <<"proof">> => <<"ReplaceMePleeeaaase">>,
    <<"transactions">> => Transactions,
    <<"previousBlockHash">> => LastBlockSha256
  }.

build_new_block_data() ->
  BlockJson = jiffy:encode(build_new_block()),
  [JsonStart, JsonEnd] = string:split(BlockJson, <<"\"ReplaceMePleeeaaase\"">>),
  {JsonStart, JsonEnd}.

