ectl -- Erlang Controller
=================

`ectl` is an escript command-line interface for some useful debugging and profiling tools for erlang VM, [redbug](https://github.com/basho/eper) and [recon](https://github.com/ferd/recon) are currently supported.

```bash
$ ./ectl
Usage: ectl  <command> [<arg>] [options]

  -h, --help     Print this help.
  -v, --version  Print the version and exit.

Available subcommands:

  proc_info
  proc_hog
  proc_win
  sysinfos
  proc_state
  redbug
  ping
  sysinfo

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

### redbug

[redbug](https://github.com/basho/eper) is similar to the OTP dbg application, but safer, better etc. `ectl` wraps this great tool in a nicer cli interface.

```bash
$ ./ectl redbug -h
Usage: ectl redbug <node> <trace_pattern> [options]

  -c, --cookie  Erlang cookie to use
  -t, --time    stop trace after this many ms [default: 15000]
  -m, --msgs    stop trace after this many msgs [default: 10]
  -p, --proc    Erlang process all|pid()|atom(RegName) [default: all]
```

To trace calls of `erlang:memory/0` on `my_app@127.0.0.1` for 10 seconds, run:

```bash
$ ./ectl redbug my_app@127.0.0.1 "erlang:memory() -> return" -c my_app

11:22:07 <{erlang,apply,2}> {erlang,memory,[]}

11:22:07 <{erlang,apply,2}> {erlang,memory,0} -> [{total,15072488},
                                                  {processes,2804020},
                                                  {processes_used,2804020},
                                                  {system,12268468},
                                                  {atom,339441},
                                                  {atom_used,322103},
                                                  {binary,598616},
                                                  {code,7314528},
                                                  {ets,529432}]
```

### ping

Ping one or more nodes in your cluster.

```bash
$ ./ectl ping -h
Usage: ectl ping <node> [...] [options]

  -c, --cookie  Erlang cookie to use
  -o, --output  output format: table|json|plain [default: plain]
```

```bash
$ ./ectl ping my_app1@127.0.0.1 my_app2@127.0.01 -c my_app
┌─────────────────────┬────────┐
│ node                │ result │
├─────────────────────┼────────┤
│ my_app1@127.0.0.1   │ pong   │
├─────────────────────┼────────┤
│ my_app2@127.0.01    │ pang   │
└─────────────────────┴────────┘
```

### sysinfo

Use [recon:node_stats_list/2](http://ferd.github.io/recon/recon.html#node_stats_list-2) to get system of a node.

```bash
$ ./ectl sysinfo -h
Usage: ectl sysinfo <node> [options]

  -c, --cookie    Erlang cookie to use
  -o, --output    output format: table|json|plain [default: plain]
  -n, --num       N time to sample stats [default: 5]
  -i, --interval  wait time for each sample [default: 1]
```

```bash
$ ./ectl sysinfo my_app@127.0.0.1 -c my_app
 bytes_in  bytes_out  err_log_q  gc_cnt  gc_reclaimed  mem_atom  mem_bin  mem_ets  mem_procs  mem_total  proc_cnt   reds  runq
        0          0          0       2          2251    353694   124112   533288    2927706   14875648        91   5681     0
    16619      16589          0      81        180749    353694   114424   533288    2990802   14929096        90  63926     0
    23146      20215          0     104        239930    353694   116448   533288    3000946   14941240        90  94567     0
    15782      16589          0      83        208024    353694   114584   533288    3005842   14944296        90  66224     0
     9591       9290          0      43         77911    353694   114664   533288    2998714   14937008        90  36095     0
```

### sysinfos

Same with [sysinfo](#sysinfo), but get info from multiple nodes.

```bash
$ ./ectl sysinfos -h
Usage: ectl sysinfos <node> [...] [options]

  -c, --cookie  Erlang cookie to use
  -o, --output  output format: table|json|plain [default: plain]
```

```bash
$ ./ectl sysinfos my_app1@127.0.0.1 my_app2@127.0.0.1 -c my_app
 bytes_in  bytes_out  err_log_q  gc_cnt  gc_reclaimed  mem_atom  mem_bin  mem_ets  mem_procs  mem_total  proc_cnt  reds  runq
        0          0          0       2          1545    354225   163272   535208    3028214   15090232        92  5667     0
        0          0          0       1            88    350697   192992   789744    4538468   17817872       169   887     0
```

### proc_info

Use [recon:info/1](http://ferd.github.io/recon/recon.html#info-1) to get info of a process of a node.

```bash
$ ./ectl proc_info -h
Usage: ectl proc_info <node> <pid> [options]

  -c, --cookie  Erlang cookie to use

```

```bash
$ ./ectl proc_info my_app@127.0.0.1 0.79.0 -c my_app
============= meta =============
 initial_call     {proc_lib,init_p,5}
 registered_name  cowboy_clock
 group_leader     <5956.76.0>
 status           waiting
============= dictionary =============
 $ancestors     [cowboy_sup,<5956.77.0>]
 $initial_call  {cowboy_clock,init,1}
============= stacktrace =============
 mod         fun              arity  file            line
 gen_server  loop             6      gen_server.erl  358
 proc_lib    init_p_do_apply  3      proc_lib.erl    227
============= memory =============
 memory             24712
 message_queue_len  0
 heap_size          2584
 total_heap_size    2961
============= garbage collection =============
 min_bin_vheap_size  46368
 min_heap_size       233
 fullsweep_after     65535
 minor_gcs           3350
============= works =============
 reductions  627628
```

### proc_state

Use [recon:get_state/1](http://ferd.github.io/recon/recon.html#get_state-1) to get state of a process.

```bash
$ ./ectl proc_state -h
Usage: ectl proc_state <node> <pid> [options]

  -c, --cookie  Erlang cookie to use

```

```bash
$ ./ectl proc_state my_app@127.0.0.1 0.79.0 -c my_app
{state,{{2013,9,29},{3,43,0}},
       <<"Sun, 29 Sep 2013 03:43:00 GMT">>,
       {interval,#Ref<5944.0.0.102>}}
```

### proc_hog

Use [recon:proc_count/2](http://ferd.github.io/recon/recon.html#proc_count-2) to get process hog, like memory hog.

```bash
$ ./ectl proc_hog -h
Usage: ectl proc_hog <node> <attr> [options]

  -c, --cookie  Erlang cookie to use
  -n, --num     number of results [default: 10]

```

```bash
$ ./ectl proc_hog my_app@127.0.0.1 memory -c my_app
 pid           name                        val  current_function                initial_call
 <5948.109.0>  sync_scanner            1801584  {gen_server,loop,6}             {proc_lib,init_p,5}
 <5948.25.0>   code_server              284360  {code_server,loop,1}            {erlang,apply,2}
 <5948.32.0>                            142752  {shell,get_command1,5}          {erlang,apply,2}
 <5948.3.0>    erl_prim_loader          142608  {erl_prim_loader,loop,3}        {erlang,apply,2}
 <5948.7.0>    application_controller    88952  {gen_server,loop,6}             {erlang,apply,2}
 <5948.24.0>   file_server_2             75816  {gen_server,loop,6}             {proc_lib,init_p,5}
 <5948.91.0>                             55040  {application_master,loop_it,4}  {application_master,start_it,4}
 <5948.11.0>   kernel_sup                34872  {gen_server,loop,6}             {proc_lib,init_p,5}
 <5948.6.0>    error_logger              29632  {gen_event,fetch_msg,5}         {proc_lib,init_p,5}
 <5948.112.0>  sync_options              29552  {gen_server,loop,6}             {proc_lib,init_p,5}
```

### proc_win

Use [recon:proc_window/3](http://ferd.github.io/recon/recon.html#proc_window-3) to sample process info in a give timespan.

```bash
$ ./ectl proc_win -h
Usage: ectl proc_win <node> [options]

  -c, --cookie  Erlang cookie to use
  -t, --time    seconds to sample [default: 10000]
  -n, --num     number of results [default: 10]
  -a, --attr    attritube to sample [default: memory]

```

```bash
$ ./ectl proc_win my_app@127.0.0.1 -c my_app
 pid             name                 val  current_function                  initial_call
 <5948.13.0>     global_name_server  7896  {gen_server,loop,6}               {proc_lib,init_p,5}
 <5948.24117.1>                      2704  {global,resolver,2}               {erlang,apply,2}
 <5948.18.0>     erl_epmd               0  {gen_server,loop,6}               {proc_lib,init_p,5}
 <5948.50.0>                            0  {application_master,main_loop,2}  {proc_lib,init_p,5}
 <5948.82.0>                            0  {application_master,main_loop,2}  {proc_lib,init_p,5}
 <5948.33.0>                            0  {gen_server,loop,6}               {proc_lib,init_p,5}
 <5948.65.0>                            0  {application_master,main_loop,2}  {proc_lib,init_p,5}
 <5948.97.0>                            0  {gen_server,loop,6}               {proc_lib,init_p,5}
 <5948.24115.1>                         0  {dist_util,con_loop,9}            {inet_tcp_dist,do_accept,6}
 <5948.16.0>     inet_db                0  {gen_server,loop,6}               {proc_lib,init_p,5}
```