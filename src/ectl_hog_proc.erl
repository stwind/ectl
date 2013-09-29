-module(ectl_hog_proc).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    ecli:start_node(ectl_lib:arg(cookie, Opts)),
    Node = list_to_atom(ecli:binding(node, Opts)),
    ectl_lib:load_recon(Node),
    Field = list_to_atom(ecli:binding(attr, Opts)),
    Num = ecli:opt(num, Opts),
    Info = rpc:call(Node, recon, proc_count, [Field, Num]),
    print_info(Info).

%% ===================================================================
%% Private
%% ===================================================================

print_info(Info) ->
    Info1 = [normalize(to_row(I)) || I <- Info],
    ecli_tbl:print(Info1, 
                   [
                    {heads,[pid,name,val,current_function,initial_call]},
                    {columns, [left,left,right,left,left]},
                    compact
                   ]).

to_row({Pid, Val, [Name | Location]}) when is_atom(Name) ->
    [{pid, Pid},{val, Val},{name, Name} | Location];
to_row({Pid, Val, Location}) ->
    [{pid, Pid},{val, Val},{name, <<>>} | Location].

normalize(Vals) ->
    [{K, ectl_lib:to_str(V)} || {K, V} <- Vals].

