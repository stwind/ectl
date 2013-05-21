-define(with(Spec, Target, Args, Next), ectl:with(Spec, cmdline(), Args, 
        fun(Opts) -> Next(Target, Opts) end)).

-define(CONSOLE(Fmt, Arg), io:format(Fmt, Arg)).
