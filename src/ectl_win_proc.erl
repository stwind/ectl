-module(ectl_win_proc).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    ecli:start_node(ectl_lib:arg(cookie, Opts)),
    Node = ectl_lib:arg(node, Opts),
    ectl_lib:load_recon(Node),
    Attr = list_to_atom(ecli:opt(attr, Opts)),
    Num = ecli:opt(num, Opts),
    Time = ecli:opt(time, Opts),
    Info = rpc:call(Node, recon, proc_window, [Attr, Num, Time]),
    print_info(Info).

%% ===================================================================
%% Private
%% ===================================================================

print_info(Info) ->
    Info1 = [normalize(to_row(I)) || I <- Info],
    ecli_tbl:print(Info1, 
                   [
                    {heads,[pid,name,val,current_function,initial_call]},
                    {columns, [left,left,right,left,left]}
                   ]).

to_row({Pid, Val, [Name | Location]}) when is_atom(Name) ->
    [{pid, Pid},{val, Val},{name, Name} | Location];
to_row({Pid, Val, Location}) ->
    [{pid, Pid},{val, Val},{name, <<>>} | Location].

normalize(Vals) ->
    [{K, ectl_lib:to_str(V)} || {K, V} <- Vals].

