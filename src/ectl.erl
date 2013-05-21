-module(ectl).

-export([subcmd/4]).
-export([run/4]).
-export([run/5]).
-export([with/4]).
-export([usage/2]).
-export([opt/2]).
-export([opt/3]).
-export([abort/0]).
-export([abort/2]).

-include("ectl.hrl").

-define(LINE_LENGTH, 75).

%% ===================================================================
%% Public
%% ===================================================================

subcmd([SubCmd | Targets], Args, Spec, Cmdline) ->
    run(cmd_mod(SubCmd), Targets, Args, Spec, Cmdline).

run(Mod, Args, Spec, Cmdline) ->
    {Targets, Args1} = targets(Args, []),
    run(Mod, Targets, Args1, Spec, Cmdline).

run(Mod, Targets, Args, Spec, Cmdline) ->
    case code:load_file(Mod) of
        {error, nofile} -> 
            usage(Spec, Cmdline);
        {module, Mod} ->
            case erlang:function_exported(Mod, run, 3) of
                true ->
                    apply(Mod, run, [Targets, Args, Spec]);
                false ->
                    usage(Spec, Cmdline)
            end
    end.

with(Spec, Cmdline, Args, After) ->
    case getopt:parse(lists:usort(Spec), Args) of
        {ok, Results} ->
            After(Results);
        {error, Error} ->
            ?CONSOLE("Invalid option sequence given: ~w~n", [Error]),
            usage(Spec, Cmdline)
    end.

usage(Spec, {Cmd, Args, SubCmds}) ->
    usage_ver(),
    usage_cmd_line(Cmd, Args),
    usage_subcmds(SubCmds),
    usage_spec(Spec).

opt(Key, Opts) ->
    opt(Key, Opts, undefined).

opt(Key, {Opts, _}, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        false -> Default;
        {Key, Value} -> Value
    end.

abort() ->
    throw(yuniocli_abort).

abort(String, Args) ->
    ?CONSOLE(String, Args),
    abort().

%% ===================================================================
%% Private
%% ===================================================================

cmd_mod(Name) ->
    {ok, Prefix} = application:get_env(ectl, cmd_mod_prefix),
    cmd_mod(Prefix, Name).

cmd_mod(Prefix, Name) ->
    list_to_atom(Prefix ++ "_" ++ Name).

targets([], Cmds) ->
    {lists:reverse(Cmds), []};
targets(["-" ++ _ | _] = Args, Cmds) ->
    {lists:reverse(Cmds), Args};
targets([Cmd | Rest], Cmds) ->
    targets(Rest, [Cmd | Cmds]).

usage_ver() ->
    {ok, App} = application:get_env(ectl, app_name),
    {ok, Ver} = application:get_key(App, vsn),
    ?CONSOLE("~p ~s~n", [App, Ver]).

usage_cmd_line(Cmd, Args) ->
    ?CONSOLE("Usage: ~s ~s [options]~n", [Cmd, Args]).

usage_spec(Spec) ->
    ?CONSOLE("~nOptions: ~n", []),
    {MaxLen, UsageLines} = usage_spec_lines(Spec, 0, []),
    MaxLineLen = line_len(),
    [print(fmt_usage_spec_line(MaxLen + 1, MaxLineLen, L)) || L <- UsageLines].

print(Line) ->
    ?CONSOLE("~s",[lists:flatten(Line)]).

usage_spec_lines([Opt | Rest], PrevMax, Acc) ->
    OptionText = usage_opt_text(Opt),
    HelpText = usage_help_text(Opt),
    {Max, ColWidth} = max_opt_length({OptionText, HelpText}, PrevMax),
    usage_spec_lines(Rest, Max, [ColWidth | Acc]);
usage_spec_lines([], Max, Acc) ->
    {Max, Acc}.

usage_opt_text({_Name, Short, undefined, _ArgSpec, _Help}) ->
    [$-, Short];
usage_opt_text({_Name, undefined, Long, _ArgSpec, _Help}) ->
    [$-, $- | Long];
usage_opt_text({_Name, Short, Long, _ArgSpec, _Help}) ->
    [$-, Short, $,, $\s, $-, $- | Long].

usage_help_text({_Name, _Short, _Long, {_ArgType, ArgValue}, [_ | _] = Help}) ->
    Help ++ " [default: " ++ default_arg_value_to_string(ArgValue) ++ "]";
usage_help_text({_Name, _Short, _Long, _ArgSpec, Help}) ->
    Help.

max_opt_length({OptText, HelpText}, PrevMax) ->
    OptLen = length(OptText),
    {erlang:max(OptLen, PrevMax), {OptLen, OptText, HelpText}}.

line_len() ->
    case io:columns() of
        {ok, Columns} when Columns < ?LINE_LENGTH ->
            Columns - 1;
        _ ->
            ?LINE_LENGTH
    end.

fmt_usage_spec_line(MaxOptLen, MaxLineLen, {OptLen, OptionText, [_ | _] = HelpText})
  when MaxOptLen < (MaxLineLen div 2) ->
    [Head | Tail] = wrap_text_line(MaxLineLen - MaxOptLen - 3, HelpText),
    FirstLineIndentation = lists:duplicate(MaxOptLen - OptLen + 1, $\s),
    Indentation = [$\n | lists:duplicate(MaxOptLen + 3, $\s)],
    ["  ", OptionText, FirstLineIndentation, Head,
     [[Indentation, Line] || Line <- Tail], $\n];
fmt_usage_spec_line(_, MaxLineLen, {_OptionLength, OptionText, [_ | _] = HelpText}) ->
    HelpLines = wrap_text_line(MaxLineLen - 6, HelpText),
    ["  ", OptionText, [["\n      ", Line] || Line <- HelpLines], $\n];
fmt_usage_spec_line(_, _, {_OptionLength, OptionText, _HelpText}) ->
    ["  ", OptionText, $\n].

wrap_text_line(Length, Text) ->
    wrap_text_line(Length, Text, [], 0, []).

wrap_text_line(Length, [Char | Tail], Acc, Count, CurrentLineAcc) when Count < Length ->
    wrap_text_line(Length, Tail, Acc, Count + 1, [Char | CurrentLineAcc]);
wrap_text_line(Length, [_ | _] = Help, Acc, Count, CurrentLineAcc) ->
    {NextLineAcc, WrappedLine} = case string:cspan(CurrentLineAcc, " \t") of
        WhitespacePos when WhitespacePos < Count ->
            lists:split(WhitespacePos, CurrentLineAcc);
        _ ->
            {[], CurrentLineAcc}
    end,
    wrap_text_line(Length, Help, [lists:reverse(WrappedLine) | Acc], length(NextLineAcc), NextLineAcc);
wrap_text_line(_Length, [], Acc, _Count, [_ | _] = CurrentLineAcc) ->
    lists:reverse([lists:reverse(CurrentLineAcc) | Acc]);
wrap_text_line(_Length, [], Acc, _Count, _CurrentLineAcc) ->
    lists:reverse(Acc).

default_arg_value_to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
default_arg_value_to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
default_arg_value_to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
default_arg_value_to_string(Value) when is_float(Value) ->
    lists:flatten(io_lib:format("~w", [Value]));
default_arg_value_to_string(Value) ->
    Value.

usage_subcmds([]) ->
    ok;
usage_subcmds(SubCmds) ->
    ?CONSOLE("~nSubcommands: ~n", []),
    {MaxLen, Lines} = usage_subcmd_lines(SubCmds, 0, []),
    MaxLineLen = line_len(),
    [print(fmt_usage_spec_line(MaxLen + 1, MaxLineLen, L)) || L <- Lines].

usage_subcmd_lines([{Cmd, Desc} | Rest], PrevMax, Acc) ->
    {Max, ColWidth} = max_opt_length({Cmd, Desc}, PrevMax),
    usage_subcmd_lines(Rest, Max, [ColWidth | Acc]);
usage_subcmd_lines([], Max, Acc) ->
    {Max, Acc}.
