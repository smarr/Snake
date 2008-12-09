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

%%
%% API Functions
%%
start(InputReceiver) ->
    processInput(InputReceiver, "").

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

