-module(ectl_hog_proc).

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
    Field = list_to_atom(ecli:binding(attr, Opts)),
    Num = ecli:opt(num, Opts),
    Info = rpc:call(Node, recon, proc_count, [Field, Num]),
    ?PRINT("~p~n",[Info]).
