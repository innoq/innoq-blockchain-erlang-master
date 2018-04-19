-module(mine_resource).
-export([
    init/1,
    content_types_provided/2,
    allowed_methods/2,
    to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

content_types_provided(RD, Ctx) ->
    {[ {"application/json", to_json} ], RD, Ctx}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD'], RD, Ctx}.

fake_proof(JsonStart, JsonEnd) ->
    {ok, <<JsonStart/binary, JsonEnd/binary>>, <<"-1">>}.

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    {JsonStart, JsonEnd} = block_utils:build_new_block_data(),
    {ok, JsonBinary, Sha256} = mining:proof(JsonStart, JsonEnd),
    state:append_to_chain(#{json => JsonBinary, sha256 => Sha256}),
    {JsonBinary, ReqData, State}.
