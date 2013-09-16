-module(ectl).

-export([main/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

main(Args) ->
    application:load(ectl),
    ecli:start(Args, spec()).

%% ===================================================================
%% Private
%% ===================================================================

spec() ->
    [
     {script, "ectl"},
     {vsn, vsn()},
     {commands, cmd()},
     {config_file, "ectl.config"}
    ].

vsn() ->
    {ok, Ver} = application:get_key(ectl, vsn),
    Ver.

cmd() ->
    [
     {"redbug", [node, trace_pattern], ectl_redbug,
      [
       {cookie, $c, "cookie", string, "Erlang cookie to use"},
       {time, $t, "time", {integer, 15000}, "stop trace after this many ms"},
       {msgs, $m, "msgs", {integer, 10}, "stop trace after this many msgs"},
       {proc, $p, "proc", {string, "all"}, "Erlang process all|pid()|atom(RegName)"}
      ]}
    ].
