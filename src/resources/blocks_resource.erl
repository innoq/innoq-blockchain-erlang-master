-module(blocks_resource).
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
    {ok, Chain} = state:get_chain_content(),
    {chain_to_json(Chain), ReqData, State}.

chain_to_json(Chain) ->
    R = #{<<"blocks">> => Chain,
          <<"blockHeight">> => length(Chain)
         },
    jiffy:encode(R, [pretty]).
