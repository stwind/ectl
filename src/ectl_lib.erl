-module(ectl_lib).

-export([get_nodes/1]).
-export([load_recon/1]).
-export([arg/2]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

load_recon(Nodes) when is_list(Nodes) ->
    [load_recon(N) || N <- Nodes];
load_recon(Node) when is_atom(Node) ->
    [recon:remote_load(Node,M) || M <- [recon,recon_lib,recon_alloc]].

get_nodes(Opts) ->
    Node = ecli:binding(node, Opts),
    Nodes = ecli:binding(others, Opts, []),
    [l2a(Node) | [l2a(N) || N <- Nodes]].

arg(pid, Opts) ->
    Pid = ecli:binding(pid, Opts),
    case string:tokens(Pid, ".") of
        [A,B,C] ->
            {l2i(A), l2i(B), l2i(C)};
        _ ->
            ?HALT("invalid pid ~p~n", [Pid])
    end;
arg(node, Opts) ->
    l2a(ecli:binding(node, Opts, ""));
arg(cookie, Opts) ->
    l2a(ecli:opt(cookie, Opts, "")).

%% ===================================================================
%% Private
%% ===================================================================

l2a(V) -> list_to_atom(V).
l2i(V) -> list_to_integer(V).
