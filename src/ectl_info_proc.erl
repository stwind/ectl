-module(ectl_info_proc).

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
    {P1,P2,P3} = ectl_lib:arg(pid, Opts),
    Info = rpc:call(Node, recon, info, [P1,P2,P3]),
    ?PRINT("~p~n",[Info]).
