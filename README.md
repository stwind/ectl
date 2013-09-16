ectl -- Erlang Controller
=================

`ectl` is an escript command-line wrapper for some useful debugging and profiling tools for erlang VM.

Currently supported [redbug](https://github.com/basho/eper).

```bash
$ ./ectl
Usage: ectl  <command> [<arg>] [options]

  -h, --help	 Print this help.
  -v, --version  Print the version and exit.

Available subcommands:

  redbug

For help on any individual command run `ectl COMMAND -h`
```

## Building

To use `ectl`, you need erlang R15B03 or later.

To build `ectl`, just clone and run:

```bash
make
```

then you will have an executable `ectl` script in the folder.

## Usage

### Redbug

[redbug](https://github.com/basho/eper) is similar to the OTP dbg application, but safer, better etc. `ectl` wraps this great tool in a nicer cli interface.

```bash
$ ./ectl redbug
Usage: ectl redbug <node> <trace_pattern> [options]

  -c, --cookie  Erlang cookie to use
  -t, --time    stop trace after this many ms [default: 15000]
  -m, --msgs    stop trace after this many msgs [default: 10]
  -p, --proc    Erlang process all|pid()|atom(RegName) [default: all]
```

To trace calls of `erlang:memory/0` on `my_app@127.0.0.1` for 10 seconds, run:
```bash
./ectl redbug my_app@127.0.0.1 "erlang:memory() -> return" -c my_cookie -t 10000
```
