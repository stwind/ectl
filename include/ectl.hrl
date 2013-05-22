-define(with(Spec, CmdLine, Targets, Args, Next), ectl:with(Spec, CmdLine, Args, 
        fun(Opts) -> apply(Next, Targets ++ [Opts])  end)).

-define(CONSOLE(Fmt, Arg), io:format(Fmt, Arg)).

-define(ABORT(Str, Args), ectl:abort(Str, Args)).
-define(HALT(Str, Args), ectl:halt(Str, Args)).
