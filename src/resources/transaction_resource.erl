-module(transaction_resource).
-export([
    init/1,
    content_types_provided/2,
    allowed_methods/2,
    resource_exists/2,
    to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, {notfound}}.

content_types_provided(RD, Ctx) ->
    {[ {"application/json", to_json} ], RD, Ctx}.

resource_exists(ReqData, Ctx) ->
    {id, Id} = lists:keyfind(id, 1, wrq:path_info(ReqData)),
    case find_transaction(list_to_binary(Id)) of
        {ok, T} -> {true, ReqData, {found, T}};
        {notfound} -> {false, ReqData, Ctx}
    end.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD'], RD, Ctx}.

to_json(ReqData, State) ->
    case State of
        {found, T} -> {jiffy:encode(T, [pretty]), ReqData, State};
        {notfound} -> {jiffy:encode(#{<<"error">> => <<"not found">>}, [pretty]), ReqData, State}
    end.

find_transaction(Id) ->
    case find_unconfirmed_transaction(Id) of
        {ok, T} -> {ok, T};
        _ -> find_confirmed_transaction(Id)
    end.

find_confirmed_transaction(Id) ->
    {ok, Chain} = state:get_chain_content(),
    Transactions = lists:flatmap(fun(Block) -> maps:get(<<"transactions">>, Block) end, Chain),
    find_transaction_in_list(Id, Transactions, true).

find_unconfirmed_transaction(Id) ->
    {ok, Transactions} = state:get_transactions(),
    Decoded = lists:map(fun(T) -> jiffy:decode(T, [return_maps]) end, Transactions),
    find_transaction_in_list(Id, Decoded, false).

find_transaction_in_list(Id, List, Confirmed) ->
    Trans = lists:filter(fun(T) -> Id == maps:get(<<"id">>, T) end, List),
    case Trans of
        [H] -> {ok, maps:put(<<"confirmed">>, Confirmed, H)};
        _ -> {notfound}
    end.
