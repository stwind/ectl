-module(ectl_lib).

-export([get_nodes/1]).
-export([load_recon/1]).
-export([arg/2]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

load_recon(Node) ->
    [recon:remote_load(Node,M) || M <- [recon,recon_lib,recon_alloc]].

get_nodes(Opts) ->
    Node = ecli:binding(node, Opts),
    Nodes = ecli:binding(others, Opts),
    parse_nodes([Node | Nodes]).

arg(pid, Opts) ->
    Pid = ecli:binding(pid, Opts),
    case string:tokens(Pid, ".") of
        [A,B,C] ->
            {l2i(A), l2i(B), l2i(C)};
        _ ->
            ?HALT("invalid pid ~p~n", [Pid])
    end;
arg(node, Opts) ->
    l2a(ecli:binding(node, Opts));
arg(cookie, Opts) ->
    l2a(ecli:opt(cookie, Opts)).

%% ===================================================================
%% Private
%% ===================================================================

l2a(V) -> list_to_atom(V).
l2i(V) -> list_to_integer(V).

parse_nodes(Nodes) ->
    [parse_node(N) || N <- Nodes].

parse_node(Node) ->
    case string:tokens(Node, ":") of
        [Name, Cookie] ->
            {l2a(Name), l2a(Cookie)};
        _ ->
            ?HALT("invalid node ~p~n", [Node])
    end.
