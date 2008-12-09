%% Author: smarr
%% Created: Dec 9, 2008
%% Description: terminal is the low level library to access the common ANSI terminal
-module(terminal).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([clean/0, set_cursor/2, put/1]).

%%
%% API Functions
%%
clean() -> 
    io:fwrite("[2J").

set_cursor(X, Y) ->
    io:fwrite("[~w;~wH", [Y, X]).

put(String) ->
	io:fwrite(String).

%%
%% Local Functions
%%
