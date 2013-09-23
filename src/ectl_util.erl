-module(ectl_util).

-export([get_nodes/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

get_nodes(Opts) ->
    Node = ecli:binding(node, Opts),
    Nodes = ecli:binding(others, Opts),
    parse_nodes([Node | Nodes]).

%% ===================================================================
%% Private
%% ===================================================================

l2a(V) ->
    list_to_atom(V).

parse_nodes(Nodes) ->
    [parse_node(N) || N <- Nodes].

parse_node(Node) ->
    case string:tokens(Node, ":") of
        [Name, Cookie] ->
            {l2a(Name), l2a(Cookie)};
        _ ->
            ?HALT("invalid node ~p~n", [Node])
    end.
