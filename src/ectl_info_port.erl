-module(ectl_info_port).

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
    Port = list_to_integer(ecli:binding(port, Opts)),
    Info = rpc:call(Node, recon, port_info, [Port]),
    ?PRINT("~p~n",[Info]).
