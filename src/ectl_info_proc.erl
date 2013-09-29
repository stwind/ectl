-module(ectl_info_proc).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    ecli:start_node(ectl_lib:arg(cookie, Opts)),
    Node = ectl_lib:arg(node, Opts),
    {P1,P2,P3} = ectl_lib:arg(pid, Opts),
    ectl_lib:load_recon(Node),
    Info = rpc:call(Node, recon, info, [P1,P2,P3]),
    case proplists:get_value(meta, Info) of
        undefined ->
            ?PRINT("proc not found");
        _ ->
            print_info(Info)
    end.

%% ===================================================================
%% Private
%% ===================================================================

print_info([{meta, Vals} | Rest]) ->
    ?PRINT("============= meta =============~n"),
    print_meta([init_call(Rest) | Vals]),
    print_info(Rest);
print_info([{location, Vals} | Rest]) ->
    print_loc(Vals),
    print_info(Rest);
print_info([{memory_used, Vals} | Rest]) ->
    print_mem(Vals),
    print_info(Rest);
print_info([{work, Vals} | Rest]) ->
    print_work(Vals),
    print_info(Rest);
print_info([_ | Rest]) ->
    print_info(Rest);
print_info([]) ->
    ok.

print_meta(Meta) ->
    Meta1 = proplists:delete(dictionary, Meta),
    ecli_tbl:print(normalize(Meta1), [compact]),
    print_dict(proplists:get_value(dictionary, Meta)).

print_dict(Dict) ->
    ?PRINT("============= dictionary =============~n"),
    ecli_tbl:print(normalize(Dict), [compact]).

print_loc(Vals) ->
    Trace = proplists:get_value(current_stacktrace, Vals),
    print_trace(Trace).

print_trace(Trace) ->
    ?PRINT("============= stacktrace =============~n"),
    Trace1 = lists:map(
      fun({M, F, A, Other}) ->
              [{mod,M},{'fun',F},{arity,A} | Other]
      end, Trace),
    ecli_tbl:print(Trace1, [{heads,[mod,'fun',arity,file,line]},compact]).

print_mem(Vals) ->
    ?PRINT("============= memory =============~n"),
    Vals1 = proplists:delete(garbage_collection, Vals),
    ecli_tbl:print(Vals1, [compact]),
    print_gc(proplists:get_value(garbage_collection, Vals)).

print_gc(Gc) ->
    ?PRINT("============= garbage collection =============~n"),
    ecli_tbl:print(Gc, [compact]).

print_work(Vals) ->
    ?PRINT("============= works =============~n"),
    ecli_tbl:print(Vals,[compact]).

normalize(Vals) ->
    [{K, ectl_lib:to_str(V)} || {K, V} <- Vals].

init_call(Info) ->
    Loc = proplists:get_value(location, Info),
    proplists:lookup(initial_call, Loc).
