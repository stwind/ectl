-module(ectl_mem).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    Results = ecli:each_node(
                fun({N, _}, _) -> 
                        Memory = rpc:call(N, erlang, memory, []),
                        [{node, N} | Memory]
                end, ectl_util:get_nodes(Opts)),
    ecli:output(Results, [heads(),columns()], Opts).

%% ===================================================================
%% Private
%% ===================================================================

heads() ->
    {heads, [node,total,processes,binary,code,system,atom,ets]}.

columns() ->
    {columns, [left,right,right,right,right,right,right,right]}.
