-module(ectl_win_proc).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    Node = ectl_lib:arg(node, Opts),
    Cookie = ectl_lib:arg(cookie, Opts),
    {ok, _} = ecli:connect_node(Node, Cookie), 
    ectl_lib:load_recon(Node),
    Attr = list_to_atom(ecli:opt(attr, Opts)),
    Num = ecli:opt(num, Opts),
    Time = ecli:opt(time, Opts),
    Info = rpc:call(Node, recon, proc_window, [Attr, Num, Time]),
    ?PRINT("~p~n",[Info]).

%% ===================================================================
%% Private
%% ===================================================================

