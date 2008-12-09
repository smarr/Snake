%% Author: smarr
%% Created: Dec 8, 2008
%% Description: Terminal reader process.
%%   - reads data from the terminal (has to be configured properli with -icanon)
%%   - sends events about input to the receiver 
-module(terminal_reader).

%%
%% Exported Functions
%%
-export([start/1]).
%% set_cursor/3, clear/1, put/2

%%
%% API Functions
%%
start(InputReceiver) ->
    clean(),
    processInput(InputReceiver, "").

%%set_cursor(X, Y, {Terminal, Server}) ->
%%    Server ! {Terminal, {command, "[" + Y + ";" + X + "H"}}.

%%clear({Terminal, Server}) ->
%%    Server ! {Terminal, {command, "[2J"}}.

%%put(String, {Terminal, Server}) ->
%%    Server ! {Terminal, {command, String}}.

%%
%% Local Functions
%%
processInput(InputReceiver, Data) ->
    CharRead = io:get_chars('', 1),
    NewData = lists:append(Data, CharRead),
    case NewData of 
        "[A" -> 
            InputReceiver ! up;
        "[B" ->
            InputReceiver ! down;
        "[C" ->
            InputReceiver ! right;
        "[D" ->
            InputReceiver ! left;
        _Else -> 
            [First|_] = NewData,
            %io:fwrite("."),
            %erlang:display(First),
            if First == 27 ->
                   if length(NewData) < 3 -> 
                      %    processInput(InputReceiver, "");
                   	  %true ->
    					  processInput(InputReceiver, NewData)
                   end;
               true ->
                   processInput(InputReceiver, "")
            end
    end,
    processInput(InputReceiver, "").


clean() -> 
    io:fwrite("[2J").
