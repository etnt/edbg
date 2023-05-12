%%% @private
%%% @hidden
%%% @end

%%
%% Code ripped and stripped from https://github.com/julianduque/erlang-color
%%
-module(edbg_color).

-export([black/1, blackb/1,
         red/1, redb/1,
         green/1, greenb/1,
         yellow/1, yellowb/1,
         blue/1, blueb/1,
         magenta/1, magentab/1,
         cyan/1, cyanb/1,
         white/1, whiteb/1
        ]).

-define(ESC, <<"\e[">>).
-define(RST, <<"0">>).
-define(BOLD, <<"1">>).
-define(SEP, <<";">>).
-define(END, <<"m">>).

%% Colors
-define(BLACK, <<"30">>).
-define(RED, <<"31">>).
-define(GREEN, <<"32">>).
-define(YELLOW, <<"33">>).
-define(BLUE, <<"34">>).
-define(MAGENTA, <<"35">>).
-define(CYAN, <<"36">>).
-define(WHITE, <<"37">>).
-define(DEFAULT, <<"39">>).

%% public API

%% @private
black(Text)    -> [color(?BLACK),    Text, reset()].
%% @private
blackb(Text)   -> [colorb(?BLACK),   Text, reset()].
%% @private
red(Text)      -> [color(?RED),      Text, reset()].
%% @private
redb(Text)     -> [colorb(?RED),     Text, reset()].
%% @private
green(Text)    -> [color(?GREEN),    Text, reset()].
%% @private
greenb(Text)   -> [colorb(?GREEN),   Text, reset()].
%% @private
yellow(Text)   -> [color(?YELLOW),   Text, reset()].
%% @private
yellowb(Text)  -> [colorb(?YELLOW),  Text, reset()].
%% @private
blue(Text)     -> [color(?BLUE),     Text, reset()].
%% @private
blueb(Text)    -> [colorb(?BLUE),    Text, reset()].
%% @private
magenta(Text)  -> [color(?MAGENTA),  Text, reset()].
%% @private
magentab(Text) -> [colorb(?MAGENTA), Text, reset()].
%% @private
cyan(Text)     -> [color(?CYAN),     Text, reset()].
%% @private
cyanb(Text)    -> [colorb(?CYAN),    Text, reset()].
%% @private
white(Text)    -> [color(?WHITE),    Text, reset()].
%% @private
whiteb(Text)   -> [colorb(?WHITE),   Text, reset()].


%% internal

%% colored text
color(Color) ->
    <<?ESC/binary, Color/binary, ?END/binary>>.

%% bold colored text
colorb(Color) ->
    <<?ESC/binary, Color/binary, ?SEP/binary, ?BOLD/binary, ?END/binary>>.

reset() ->
    <<?ESC/binary, ?RST/binary, ?END/binary>>.
