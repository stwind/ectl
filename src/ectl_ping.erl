-module(ectl_ping).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    ecli:start_node(ectl_lib:arg(cookie, Opts)),
    Nodes = ectl_lib:get_nodes(Opts),
    {Good, Bad} = rpc:multicall(Nodes, erlang, node, [], 10000),
    Results = [[{node,N},{result,pong}] || N <- Good] ++ 
    [[{node,N},{result,pang}] || N <- Bad],
    ecli_tbl:print(Results, []).

%% ===================================================================
%% Private
%% ===================================================================

%heads() ->
    %{heads, [node, result]}.
