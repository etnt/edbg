#!/usr/bin/env bash

# EDBG - Erlang Debugger/Tracer Presentation
# A runnable terminal-based slide presentation

set -e

# Color definitions
BOLD='\033[1m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
RED='\033[0;31m'
RESET='\033[0m'

# Slide counter
CURRENT_SLIDE=1
TOTAL_SLIDES=14

# Clear screen function
clear_screen() {
    clear
}

# Display header
show_header() {
    echo -e "${BOLD}${CYAN}"
    echo "╔════════════════════════════════════════════════════════════════════════╗"
    echo "║                         EDBG PRESENTATION                              ║"
    echo "║                   Erlang Debugger & Tracer                            ║"
    echo "╚════════════════════════════════════════════════════════════════════════╝"
    echo -e "${RESET}"
}

# Display footer with navigation
show_footer() {
    echo ""
    echo -e "${BOLD}────────────────────────────────────────────────────────────────────────${RESET}"
    echo -e "${YELLOW}Slide $CURRENT_SLIDE of $TOTAL_SLIDES${RESET}"
    echo ""
    echo -e "${GREEN}[Enter]${RESET} Next  ${GREEN}[p]${RESET} Previous  ${GREEN}[g]${RESET} Go to slide  ${GREEN}[q]${RESET} Quit"
    echo -n "> "
}

# Slide 1: Title & Introduction
slide_1() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}EDBG - TTY-Based Erlang Debugger & Tracer${RESET}"
    echo ""
    echo -e "${CYAN}What is EDBG?${RESET}"
    echo "  • A terminal-based interface to Erlang debugging and tracing"
    echo "  • Perfect for developers who prefer CLI over GUIs"
    echo "  • Ideal for remote debugging without GUI forwarding"
    echo "  • Simple and intuitive interface to BEAM tracing"
    echo ""
    echo -e "${CYAN}Key Features:${RESET}"
    echo -e "  1. ${BOLD}Tracing${RESET} - Clean function call chains without drowning in output"
    echo -e "  2. ${BOLD}Supervisor Browser${RESET} - Navigate process trees interactively"
    echo -e "  3. ${BOLD}Process Tree Visualization${RESET} - See linked process relationships"
    echo -e "  4. ${BOLD}Debugger${RESET} - TTY-based single-stepping through code"
    echo ""
    show_footer
}

# Slide 2: History
slide_2() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}History & Background${RESET}"
    echo ""
    echo -e "${YELLOW}The Origins:${RESET}"
    echo "  • Concept dates back to early 1990s"
    echo "  • Proof-of-concept for Erlang debugger's lower API"
    echo "  • Magnus wrote the first Erlang debugger (interpreter)"
    echo ""
    echo -e "${YELLOW}Modern Era:${RESET}"
    echo "  • Fast-forward 30 years..."
    echo "  • Recreated as a modern tool for today's developers"
    echo "  • Built upon solid BEAM tracing foundation"
    echo ""
    echo -e "${CYAN}Why EDBG?${RESET}"
    echo "  • Many developers prefer terminal-based workflows"
    echo "  • Remote work requires efficient remote debugging"
    echo "  • Avoid the complexity of GUI forwarding over SSH"
    echo "  • Simple, focused, and powerful"
    echo ""
    show_footer
}

# Slide 3: Tracing Overview
slide_3() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}TRACING - The Core Feature${RESET}"
    echo ""
    echo -e "${YELLOW}The Problem:${RESET}"
    echo "  • BEAM can generate MASSIVE amounts of trace output"
    echo "  • Easy to get drowned in information overload"
    echo "  • Hard to follow execution flow"
    echo ""
    echo -e "${GREEN}The EDBG Solution:${RESET}"
    echo "  • Display function calls as a clean call chain"
    echo "  • Indentation shows call depth"
    echo "  • Arguments hidden by default (inspect on demand)"
    echo "  • Each line numbered for easy reference"
    echo "  • Process IDs shown for concurrent execution tracking"
    echo ""
    echo -e "${CYAN}Result:${RESET}"
    echo "  ✓ Clean, readable output"
    echo "  ✓ Easy to spot interesting function calls"
    echo "  ✓ Drill down into specific arguments when needed"
    echo "  ✓ Follow execution across processes"
    echo ""
    show_footer
}

# Slide 4: Basic Tracing Commands
slide_4() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Tracing: Getting Started${RESET}"
    echo ""
    echo -e "${GREEN}1. Start tracing:${RESET}"
    echo -e "   ${CYAN}edbg:fstart([ModuleList], Options)${RESET}"
    echo "   Example:"
    echo -e "   ${YELLOW}edbg:fstart([yaws_server, yaws, yaws_config], [{max_msgs, 10000}])${RESET}"
    echo ""
    echo -e "${GREEN}2. Run your test/application${RESET}"
    echo "   (Generate the trace data you want to analyze)"
    echo ""
    echo -e "${GREEN}3. Stop tracing:${RESET}"
    echo -e "   ${CYAN}edbg:fstop()${RESET}"
    echo ""
    echo -e "${GREEN}4. View trace output:${RESET}"
    echo -e "   ${CYAN}edbg:file()${RESET}"
    echo ""
    echo -e "${BLUE}Trace output saved to: ${YELLOW}edbg.trace_result${RESET} (default)"
    echo ""
    show_footer
}

# Slide 5: Trace Output Example
slide_5() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Tracing: Output Format${RESET}"
    echo ""
    echo -e "${CYAN}Clean, Indented Call Chain:${RESET}"
    echo ""
    echo "   0: <0.255.0> yaws_server:gserv_loop/4"
    echo "   1:  <0.258.0> yaws_server:gserv_loop/4"
    echo "   2:   <0.233.0> yaws:month/1"
    echo "   4:   <0.259.0> yaws_server:peername/2"
    echo "   6:   <0.258.0> yaws_server:close_accepted_if_max/2"
    echo "   8:   <0.258.0> yaws_server:acceptor/1"
    echo "  10:   <0.258.0> yaws_server:gserv_loop/4"
    echo "  11:    <0.281.0> yaws_server:acceptor0/2"
    echo "  12:     <0.281.0> yaws_server:do_accept/1"
    echo ""
    echo -e "${YELLOW}Key Elements:${RESET}"
    echo -e "  • ${GREEN}Line number${RESET} - For referencing specific calls"
    echo -e "  • ${GREEN}Indentation${RESET} - Shows call depth"
    echo -e "  • ${GREEN}Process ID${RESET} - Track which process executes what"
    echo -e "  • ${GREEN}Module:Function/Arity${RESET} - What's being called"
    echo ""
    show_footer
}

# Slide 6: Inspecting Trace Data
slide_6() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Tracing: Interactive Commands${RESET}"
    echo ""
    echo -e "${GREEN}Inspect Arguments:${RESET}"
    echo -e "   ${CYAN}s 4${RESET}           Show function clause heads"
    echo -e "   ${CYAN}s 4 2${RESET}         Show argument 2 of line 4"
    echo ""
    echo -e "${GREEN}Inspect Return Values:${RESET}"
    echo -e "   ${CYAN}r 4${RESET}           Show return value of line 4"
    echo ""
    echo -e "${GREEN}Pretty Print Records:${RESET}"
    echo -e "   ${CYAN}pr 177 2${RESET}      Pretty print record in argument 2"
    echo ""
    echo -e "${GREEN}Navigation:${RESET}"
    echo -e "   ${CYAN}a 10${RESET}          Go to line 10"
    echo -e "   ${CYAN}u${RESET}             Scroll up"
    echo -e "   ${CYAN}d${RESET}             Scroll down"
    echo -e "   ${CYAN}p 50${RESET}          Set page size to 50 lines"
    echo ""
    show_footer
}

# Slide 7: Advanced Tracing - Search
slide_7() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Tracing: Search & Filter${RESET}"
    echo ""
    echo -e "${GREEN}Search Function Calls (RegExp):${RESET}"
    echo -e "   ${CYAN}f yaws:decode_b${RESET}"
    echo "   Finds: yaws:decode_base64/1, yaws:decode_base64/2, etc."
    echo ""
    echo -e "${GREEN}Search in Arguments:${RESET}"
    echo -e "   ${CYAN}f yaws:setopts 2 packet_size${RESET}"
    echo "   Searches argument 2 for 'packet_size'"
    echo ""
    echo -e "${GREEN}Search Return Values:${RESET}"
    echo -e "   ${CYAN}fr GET${RESET}"
    echo "   Finds calls returning values containing 'GET'"
    echo ""
    echo -e "${GREEN}Follow/Unfollow Process:${RESET}"
    echo -e "   ${CYAN}fp${RESET}            Follow current process (filter by PID)"
    echo -e "   ${CYAN}up${RESET}            Unfollow process"
    echo ""
    show_footer
}

# Slide 8: Advanced Tracing Options
slide_8() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Tracing: Advanced Options${RESET}"
    echo ""
    echo -e "${CYAN}monotonic_ts${RESET} - Show elapsed time in nanoseconds"
    echo "   0: <0.297.0> yaws_server:peername/2 - 0"
    echo "   2: <0.296.0> yaws_server:close_accepted_if_max/2 - 37276"
    echo ""
    echo -e "${CYAN}send_receive${RESET} - Track messages between processes"
    echo "   6: >>> Send(<0.652.0>) -> To(<0.626.0>)  {'$gen_call',..."
    echo "   7: <<< Receive(<0.652.0>)  {#Ref<0.733...}..."
    echo ""
    echo -e "${CYAN}memory${RESET} - Track memory usage of processes"
    echo "   0: <0.297.0>(14016) yaws_server:peername/2"
    echo "   2: <0.296.0>(21848) yaws_server:close_accepted_if_max/2"
    echo ""
    echo -e "${CYAN}Trace a specific process:${RESET}"
    echo -e "   ${YELLOW}edbg:fpid(Pid)${RESET}  or  ${YELLOW}edbg:fpid(Pid, Options)${RESET}"
    echo ""
    show_footer
}

# Slide 9: Evaluate Expressions
slide_9() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Tracing: Evaluate Expressions${RESET}"
    echo ""
    echo -e "${GREEN}Bind Variables:${RESET}"
    echo -e "   ${CYAN}set Keys 304 1${RESET}        Bind Keys to argument 1 of line 304"
    echo -e "   ${CYAN}set X 20645${RESET}           Bind X to return value of line 20645"
    echo ""
    echo -e "${GREEN}Evaluate Expressions:${RESET}"
    echo -e "   ${CYAN}let MyKeys [status|Keys].${RESET}"
    echo -e "   ${CYAN}eval erlang:length(X).${RESET}"
    echo -e "   ${CYAN}eval restconf:keymember(MyKeys, 1, Tuples).${RESET}"
    echo ""
    echo -e "${GREEN}Extract Record Fields:${RESET}"
    echo -e "   ${CYAN}set IpPort 24 1 #arg.client_ip_port${RESET}"
    echo ""
    echo -e "${GREEN}Export All Functions (for testing):${RESET}"
    echo -e "   ${CYAN}xall restconf${RESET}         Recompile with export_all"
    echo -e "   ${CYAN}xnall restconf${RESET}        Reload original module"
    echo ""
    show_footer
}

# Slide 10: Elixir Support
slide_10() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Elixir Support${RESET}"
    echo ""
    echo -e "${CYAN}EDBG works seamlessly with Elixir!${RESET}"
    echo ""
    echo -e "${GREEN}Starting from iex shell:${RESET}"
    echo -e "   ${YELLOW}:edbg.fstart([Elixir.MyApp, Elixir.MyModule],"
    echo -e "                max_msgs: 1000000)${RESET}"
    echo ""
    echo -e "${GREEN}Output looks similar:${RESET}"
    echo "   14: <0.698.0> 'Elixir.TableTennis.Repo':all/2"
    echo "   15:  <0.698.0> 'Elixir.TableTennis.Repo':default_options/1"
    echo "   28:     <0.698.0> 'Elixir.TableTennisWeb.PlayerController':index/2"
    echo ""
    echo -e "${GREEN}Works on Nerves devices:${RESET}"
    echo -e "   ${CYAN}:edbg.fstart([Circuits.I2C],"
    echo -e "                max_msgs: 10000,"
    echo -e "                log_file: '/data/edbg.trace',"
    echo -e "                cfg_file: false)${RESET}"
    echo ""
    show_footer
}

# Slide 11: Supervisor Tree Browser
slide_11() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Supervisor Tree Browser${RESET}"
    echo ""
    echo -e "${CYAN}Interactive exploration of supervisor trees:${RESET}"
    echo ""
    echo -e "${GREEN}Start browser:${RESET}"
    echo -e "   ${CYAN}edbg:suptrees()${RESET}"
    echo ""
    echo -e "${GREEN}Commands:${RESET}"
    echo -e "   ${CYAN}x 23${RESET}          Expand supervisor 23"
    echo -e "   ${CYAN}s 23${RESET}          Shrink supervisor 23"
    echo -e "   ${CYAN}p 24${RESET}          Show process info"
    echo -e "   ${CYAN}p 24 2${RESET}        Show linked process info"
    echo -e "   ${CYAN}b 24${RESET}          Show process backtrace"
    echo -e "   ${CYAN}g 4${RESET}           Show gen_server state (pretty printed)"
    echo -e "   ${CYAN}m 161${RESET}         Monitor process"
    echo -e "   ${CYAN}ts 4${RESET}          Trace start on process"
    echo -e "   ${CYAN}te${RESET}            Trace end"
    echo -e "   ${CYAN}tf${RESET}            Trace show file"
    echo ""
    show_footer
}

# Slide 12: Process Tree Visualization
slide_12() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Process Tree Visualization${RESET}"
    echo ""
    echo -e "${CYAN}Generate visual process link trees:${RESET}"
    echo ""
    echo -e "${GREEN}Usage:${RESET}"
    echo -e "   ${CYAN}edbg:pp_tree(Pid)${RESET}"
    echo -e "   ${CYAN}edbg:pp_tree(Pid, [current_function, registered_name])${RESET}"
    echo -e "   ${CYAN}edbg:pp_tree(Pid, MaxDepth, ItemsList)${RESET}"
    echo ""
    echo -e "${GREEN}Example Output:${RESET}"
    echo "   <0.49.0> , current=erlang:hibernate/3, regname=kernel_sup"
    echo "     |--<0.52.0> , current=gen_server:loop/7, regname=rex"
    echo "     |--<0.63.0> , current=gen_server:loop/7, trap_exit"
    echo "     |  +--<0.65.0> , current=group:server_loop/3, regname=user"
    echo "     |     +--<0.64.0> , current=user_drv:server_loop/6"
    echo ""
    echo -e "${BLUE}Handles cycles, prevents infinite recursion${RESET}"
    echo ""
    show_footer
}

# Slide 13: Debugger (Brief Overview)
slide_13() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Debugger - Interactive Stepping${RESET}"
    echo ""
    echo -e "${YELLOW}Note: Tracing is usually preferred in production${RESET}"
    echo "      (timers may expire during stepping)"
    echo ""
    echo -e "${GREEN}Basic Commands:${RESET}"
    echo -e "   ${CYAN}edbg:i(Module, LineNum)${RESET}     Set breakpoint"
    echo -e "   ${CYAN}edbg:plist()${RESET}                List debugged processes"
    echo -e "   ${CYAN}edbg:attach(0,648,0)${RESET}        Attach to process"
    echo ""
    echo -e "${GREEN}Interactive Commands:${RESET}"
    echo "   n      - Next (step over)"
    echo "   s      - Step (into)"
    echo "   f      - Finish (step out)"
    echo "   c      - Continue"
    echo "   v Var  - Show variable value"
    echo "   pr Var - Pretty print record"
    echo "   e Expr - Evaluate expression"
    echo ""
    show_footer
}

# Slide 14: Summary & Resources
slide_14() {
    clear_screen
    show_header
    echo -e "${BOLD}${MAGENTA}Summary & Getting Started${RESET}"
    echo ""
    echo -e "${CYAN}Key Takeaways:${RESET}"
    echo -e "  ✓ ${BOLD}Tracing${RESET} is the killer feature - clean, powerful, practical"
    echo -e "  ✓ ${BOLD}Supervisor Browser${RESET} for system exploration"
    echo -e "  ✓ ${BOLD}Process Trees${RESET} for understanding relationships"
    echo -e "  ✓ ${BOLD}Debugger${RESET} for when you really need stepping"
    echo ""
    echo -e "${GREEN}Quick Start:${RESET}"
    echo -e "   ${YELLOW}1> edbg:fstart([my_module], [{max_msgs, 10000}]).${RESET}"
    echo -e "   ${YELLOW}2> % Run your code here${RESET}"
    echo -e "   ${YELLOW}3> edbg:fstop().${RESET}"
    echo -e "   ${YELLOW}4> edbg:file().${RESET}"
    echo ""
    echo -e "${CYAN}Get Help:${RESET}"
    echo -e "   ${CYAN}edbg:fhelp()${RESET}            Show command reference"
    echo ""
    echo -e "${GREEN}Documentation: Check the wiki/ directory${RESET}"
    echo ""
    show_footer
}

# Navigation function
navigate() {
    while true; do
        case $CURRENT_SLIDE in
            1) slide_1 ;;
            2) slide_2 ;;
            3) slide_3 ;;
            4) slide_4 ;;
            5) slide_5 ;;
            6) slide_6 ;;
            7) slide_7 ;;
            8) slide_8 ;;
            9) slide_9 ;;
            10) slide_10 ;;
            11) slide_11 ;;
            12) slide_12 ;;
            13) slide_13 ;;
            14) slide_14 ;;
        esac

        read -r input
        case $input in
            n|N|"")
                if [ $CURRENT_SLIDE -lt $TOTAL_SLIDES ]; then
                    CURRENT_SLIDE=$((CURRENT_SLIDE + 1))
                fi
                ;;
            p|P)
                if [ $CURRENT_SLIDE -gt 1 ]; then
                    CURRENT_SLIDE=$((CURRENT_SLIDE - 1))
                fi
                ;;
            g|G)
                echo -n "Go to slide (1-$TOTAL_SLIDES): "
                read -r slide_num
                if [ "$slide_num" -ge 1 ] && [ "$slide_num" -le $TOTAL_SLIDES ]; then
                    CURRENT_SLIDE=$slide_num
                fi
                ;;
            q|Q)
                clear_screen
                echo -e "${GREEN}Thank you for viewing the EDBG presentation!${RESET}"
                echo ""
                exit 0
                ;;
        esac
    done
}

# Main execution
main() {
    echo -e "${CYAN}Welcome to the EDBG Presentation!${RESET}"
    echo ""
    echo "Press ENTER to start..."
    read -r
    navigate
}

main
