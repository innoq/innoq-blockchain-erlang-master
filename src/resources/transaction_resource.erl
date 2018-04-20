-module(transaction_resource).
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

to_json(ReqData, State) ->
    {id, Id} = lists:keyfind(id, 1, wrq:path_info(ReqData)),
    case find_transaction(list_to_binary(Id)) of
        {ok, T} -> {jiffy:encode(T, [pretty]), ReqData, State};
        {notfound} -> {jiffy:encode(#{<<"error">> => <<"not found">>}, [pretty]), ReqData, State}
    end.

find_transaction(Id) ->
    case find_confirmed_transaction(Id) of
        {ok, T} ->
            {ok, maps:put(<<"confirmed">>, true, T)};
        _ -> find_unconfirmed_transaction(Id)
    end.

find_confirmed_transaction(Id) ->
    {ok, Chain} = state:get_chain_content(),
    Transactions = lists:flatmap(fun(Block) -> maps:get(<<"transactions">>, Block) end, Chain),
    Trans = lists:filter(fun(T) -> Id == maps:get(<<"id">>, T) end, Transactions),
    case Trans of
        [H] -> {ok, H};
        _ -> {notfound}
    end.

find_unconfirmed_transaction(Id) ->
    {ok, Transactions} = state:get_transactions(),
    Trans = lists:filter(fun({T}) -> case proplist:lookup(<<"id">>, T) of
                                         none -> false;
                                         _ -> true
                                     end
                         end, Transactions),
    case Trans of
        [H|T] -> {ok, {lists:append(H, [{<<"confirmed">>, false}])}};
        _ -> {notfound}
    end.
