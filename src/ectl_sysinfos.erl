-module(ectl_sysinfos).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    ecli:start_node(ectl_lib:arg(cookie, Opts)),
    Nodes = ectl_lib:get_nodes(Opts),
    ectl_lib:load_recon(Nodes),
    {Results, _} = rpc:multicall(Nodes, recon, node_stats_list, [1, 1000], 10000),
    print_res(Results).

%% ===================================================================
%% Private
%% ===================================================================

print_res(Results) ->
    Res1 = [short(R1 ++ R2, []) || [{R1, R2}] <- Results],
    ecli_tbl:print(Res1, [{columns, cols(Res1)}]).

short([{process_count, V} | Rest], Acc) ->
    short(Rest, [{proc_cnt, V} | Acc]);
short([{run_queue, V} | Rest], Acc) ->
    short(Rest, [{runq, V} | Acc]);
short([{error_logger_queue_len, V} | Rest], Acc) ->
    short(Rest, [{err_log_q, V} | Acc]);
short([{memory_total, V} | Rest], Acc) ->
    short(Rest, [{mem_total, V} | Acc]);
short([{memory_procs, V} | Rest], Acc) ->
    short(Rest, [{mem_procs, V} | Acc]);
short([{memory_atoms, V} | Rest], Acc) ->
    short(Rest, [{mem_atom, V} | Acc]);
short([{memory_bin, V} | Rest], Acc) ->
    short(Rest, [{mem_bin, V} | Acc]);
short([{memory_ets, V} | Rest], Acc) ->
    short(Rest, [{mem_ets, V} | Acc]);
short([{bytes_in, V} | Rest], Acc) ->
    short(Rest, [{bytes_in, V} | Acc]);
short([{bytes_out, V} | Rest], Acc) ->
    short(Rest, [{bytes_out, V} | Acc]);
short([{gc_count, V} | Rest], Acc) ->
    short(Rest, [{gc_cnt, V} | Acc]);
short([{gc_words_reclaimed, V} | Rest], Acc) ->
    short(Rest, [{gc_reclaimed, V} | Acc]);
short([{reductions, V} | Rest], Acc) ->
    short(Rest, [{reds, V} | Acc]);
short([_ | Rest], Acc) ->
    short(Rest, Acc);
short([], Acc) ->
    Acc.

cols([R | _]) ->
    [right || _ <- R].
