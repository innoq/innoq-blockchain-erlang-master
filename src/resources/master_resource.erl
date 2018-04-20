-module(master_resource).
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

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    {ok, NextIndex} = state:get_next_index(),
    {ok, Transactions} = state:get_transactions(),
    {jiffy:encode(#{
        <<"nodeId">> => <<"4711-ncc-1901">>,
        <<"currentBlockHeight">> => NextIndex,
        <<"internal">> => #{
            <<"current_transactions">> => Transactions,
            <<"nodes">> => lists:map(fun({Name, LoadFactor}) ->
                #{
                    <<"name">> => list_to_binary(Name),
                    <<"load_factor">> => LoadFactor
                }
            end, mining:get_nodes())
        }
    }), ReqData, State}.
