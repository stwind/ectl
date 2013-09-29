-module(ectl_proc_state).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    ecli:start_node(ectl_lib:arg(cookie, Opts)),
    Node = ectl_lib:arg(node, Opts),
    ectl_lib:load_recon(Node),
    Pid = ectl_lib:arg(pid, Opts),
    Info = rpc:call(Node, recon, get_state, [Pid]),
    ?PRINT("~p~n",[Info]).
