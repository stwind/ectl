-module(ectl_proc_state).

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
    Pid = ectl_lib:arg(pid, Opts),
    Info = rpc:call(Node, recon, get_state, [Pid]),
    ?PRINT("~p~n",[Info]).
