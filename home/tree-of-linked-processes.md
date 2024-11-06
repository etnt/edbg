# Tree of linked processes

This function generates a visual representation of the linked Erlang processes in your system. Starting from a given Process ID, it traverses the process tree, displaying each process and its linked children with clear indentation to highlight the hierarchical relationships.

The function is designed to handle cycles in the process links,by avoiding visting already visited processes, preventing infinite recursion.

Apart from the starting node you can also restrict the max depth of the tree and you can specify a list of items to be printed for each process. These items are the key values that you get in return from the `erlang:process_info/1` function.

* `edbg:pp_tree(StartPid)`
* `edbg:pp_tree(StartPid, ItemsList)`
* `edbg:pp_tree(StartPid, MaxDepth, ItemsList)`

The function returns: `{ok, NumOfLinesPrinted}`

Example:

```erlang
1> edbg:pp_tree(pid(0,49,0), [current_function,trap_exit,registered_name]).
<0.49.0> , current=erlang:hibernate/3, trap_exit, regname=kernel_sup
  |--<0.52.0> , current=gen_server:loop/7, trap_exit, regname=rex
  |--<0.60.0> , current=gen_event:fetch_msg/6, trap_exit, regname=erl_signal_server
  |--<0.63.0> , current=gen_server:loop/7, trap_exit
  |  +--<0.65.0> , current=group:server_loop/3, trap_exit, regname=user
  |     +--<0.64.0> , current=user_drv:server_loop/6, trap_exit, regname=user_drv
  |        |--<0.66.0> , current=group:server_loop/3, trap_exit
  |        |  +--<0.79.0> , current=shell:shell_rep/4, trap_exit
  |        |     +--<0.80.0> , current=edbg_pp_tree:mk_xinfo/2
  |        +--#Port<0.3> 
  |--<0.68.0> , current=gen_server:loop/7, regname=kernel_refc
  |--<0.69.0> , current=erlang:hibernate/3, trap_exit, regname=kernel_safe_sup
  |--<0.70.0> , current=gen_server:loop/7, trap_exit, regname=logger_sup
  |  |--<0.71.0> , current=gen_server:loop/7, trap_exit, regname=logger_handler_watcher
  |  |--<0.72.0> , current=gen_server:loop/7, trap_exit, regname=logger_proxy
  |  +--<0.74.0> , current=gen_server:loop/7, trap_exit, regname=logger_std_h_default
  |     +--<0.75.0> , current=logger_std_h:file_ctrl_loop/1
  |--<0.67.0> , current=gen_server:loop/7, trap_exit
  |--<0.61.0> , current=gen_server:loop/7, trap_exit, regname=standard_error_sup
  |  +--<0.62.0> , current=standard_error:server_loop/1, trap_exit, regname=standard_error
  |     +--#Port<0.2> 
  |--<0.57.0> , current=gen_server:loop/7, trap_exit, regname=global_group
  |  +--<0.58.0> , current=global_group:global_group_check_dispatcher/0, regname=global_group_check
  |--<0.59.0> , current=gen_server:loop/7, trap_exit, regname=file_server_2
  |--<0.54.0> , current=gen_server:loop/7, trap_exit, regname=global_name_server
  |  |--<0.55.0> , current=global:loop_the_locker/1, trap_exit
  |  +--<0.56.0> , current=global:loop_the_registrar/0
  |--<0.50.0> , current=code_server:loop/1, trap_exit, regname=code_server
  |--<0.51.0> , current=gen_server:loop/7, trap_exit, regname=inet_db
  +--<0.47.0> , current=application_master:loop_it/4, trap_exit
     +--<0.46.0> , current=application_master:main_loop/2, trap_exit
        +--<0.44.0> , current=gen_server:loop/7, trap_exit, regname=application_controller
           +--<0.0.0> , current=init:loop/1, trap_exit, regname=init
              |--<0.42.0> , current=gen_server:loop/7, trap_exit, regname=logger
              +--<0.10.0> , current=erl_prim_loader:loop/3, trap_exit, regname=erl_prim_loader
{ok,35}
```
