-module(ectl_redbug).

-export([run/1]).

-include("ectl.hrl").

%% ===================================================================
%% Public
%% ===================================================================

run(Opts) ->
    Target = target(Opts),
    ecli:connect_node(Target, cookie(Opts)),
    ecli:wait_for(redbug_start(
                    trc(ecli:binding(trace_pattern, Opts)),
                    [
                     {proc, proc(Opts)},
                     {time, ecli:opt(time, Opts)},
                     {msgs, ecli:opt(msgs, Opts)},
                     {target, Target}
                    ]
                   )).

%% ===================================================================
%% Public
%% ===================================================================

trc("send") -> send;
trc("receive") -> 'receive';
trc(RTP) -> RTP.

cookie(Opts) ->
    list_to_atom(ecli:opt(cookie, Opts, "undef_cookie")).

proc(Opts) ->
    prc(ecli:opt(procs, Opts, "all")).

to_int(V) ->
    list_to_integer(V).

prc("all") -> 
    all;
prc("0." ++ _ = Pid) ->
    case string:tokens(Pid, ".") of
        ["0",I1,I2] ->
            {pid, to_int(I1), to_int(I2)};
        _ ->
            ecli:halt_with("invalid pid ~p~n",[Pid])
    end;
prc(Name) ->
    list_to_atom(Name).

target(Opts) ->
    list_to_atom(ecli:binding(node, Opts)).

redbug_start(Trc, Props) ->
    spawn(
      fun() ->
              redbug:start(Trc, Props),
              link(whereis(redbug)),
              receive
                  done -> ok
              end
      end).
