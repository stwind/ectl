-module(ectl_ping).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    Node = ecli:binding(node, Opts),
    case ecli:connect_node(list_to_atom(Node), cookie(Opts)) of
        {ok, _} ->
            ?PRINT("pong");
        {error, _} ->
            ?PRINT("pang")
    end.

%% ===================================================================
%% Private
%% ===================================================================

cookie(Opts) ->
    list_to_atom(ecli:opt(cookie, Opts, "undef_cookie")).
