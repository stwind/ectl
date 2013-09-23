-module(ectl_sysinfo).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    Results = ecli:each_node(
                fun({N, _}, pong) -> get_sysinfo(N);
                   ({N, _}, pang) -> [{node, N}]
                end, ectl_util:get_nodes(Opts)),
    ecli:output(Results, [heads(),columns()], Opts).

%% ===================================================================
%% Private
%% ===================================================================

heads() ->
    {heads, [node,num_procs,mem_total,mem_procs,mem_bin,mem_sys]}.

columns() ->
    {columns, [left,right,right,right,right,right]}.

get_sysinfo(Node) ->
    [{node, Node}, get_proc_count(Node)] ++
    get_mem(Node).

get_mem(Node) ->
    Memory = rpc:call(Node, erlang, memory, []),
    [
     {mem_total, proplists:get_value(total, Memory)},
     {mem_procs, proplists:get_value(processes, Memory)},
     {mem_bin, proplists:get_value(binary, Memory)},
     {mem_sys, proplists:get_value(system, Memory)}
    ].

get_proc_count(Node) ->
    Count = rpc:call(Node, erlang, system_info, [process_count]),
    {num_procs, Count}.
