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
     {"ping", [node, '...'], ectl_ping,
      [
       opt(cookie), ?OPT_OUTPUT
      ]},

     {"sysinfo", [node], ectl_sysinfo,
      [
       opt(cookie), ?OPT_OUTPUT,
       {num, $n, "num", {integer, 5}, "N time to sample stats"},
       {interval, $i, "interval", {integer, 1}, "wait time for each sample"}
      ]},

     {"proc_info", [node, pid], ectl_info_proc,
      [
       opt(cookie)
      ]},

     %{"port_info", [node, port], ectl_info_port,
      %[
       %opt(cookie)
      %]},
     %
     {"proc_hog", [node, attr], ectl_hog_proc,
      [
       opt(cookie),
       {num, $n, "num", {integer, 10}, "number of results"}
      ]},

     {"port_hog", [node, attr], ectl_hog_port,
      [
       opt(cookie)
      ]},

     {"proc_win", [node], ectl_win_proc, 
      [
       opt(cookie),
       {time, $t, "time", {integer, 10000}, "seconds to sample"},
       {num, $n, "num", {integer, 10}, "number of results"},
       {attr, $a, "attr", {string, "memory"}, "attritube to sample"}
      ]},

     {"proc_state", [node, pid], ectl_proc_state, 
      [ 
       opt(cookie)
      ]},

     {"redbug", [node, trace_pattern], ectl_redbug,
      [
       opt(cookie),
       {time, $t, "time", {integer, 15000}, "stop trace after this many ms"},
       {msgs, $m, "msgs", {integer, 10}, "stop trace after this many msgs"},
       {proc, $p, "proc", {string, "all"}, "Erlang process all|pid()|atom(RegName)"}
      ]}
    ].

opt(cookie) ->
    {cookie, $c, "cookie", string, "Erlang cookie to use"}.
