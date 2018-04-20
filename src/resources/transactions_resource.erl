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
    NewTransaction = as_transaction(Payload),
    state:append_transaction(NewTransaction),
    {PropList} = NewTransaction,
    UnconfirmedPropList = lists:append(PropList, [{<<"confirmed">>, false}]),
    {true, wrq:append_to_response_body(jiffy:encode({UnconfirmedPropList}), RD), Ctx}.

as_transaction(Payload) ->
    U = uuid:to_string(uuid:v4()),
    {[{<<"id">>, list_to_binary(U)},
      {<<"timestamp">>, unixtime:gettime()},
      {<<"payload">>, Payload}
     ]}.
