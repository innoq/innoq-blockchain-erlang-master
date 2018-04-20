-module(transactions_resource).
-export([
    init/1,
    content_types_provided/2,
    allowed_methods/2,
    process_post/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

content_types_provided(RD, Ctx) ->
    {[ {"application/json", to_json} ], RD, Ctx}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

process_post(RD, Ctx) ->
    Payload = maps:get(<<"payload">>, jiffy:decode(wrq:req_body(RD), [return_maps])),
    state:append_transaction(Payload),
    {true, wrq:append_to_response_body("foo", RD), Ctx}.

