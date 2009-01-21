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
    %% clear message buffer
    receive
        after 0 ->
            null
    end,
    CharRead = io:get_chars('', 1),
    NewData = lists:append(Data, CharRead),
    case NewData of 
        "[A" -> 
            snake:move_up(InputReceiver);
        "[B" ->
            snake:move_down(InputReceiver);
        "[C" ->
            snake:move_right(InputReceiver);
        "[D" ->
            snake:move_left(InputReceiver);
        "q" ->
            InputReceiver ! quit;
        _Else -> 
            [First|_] = NewData,
            %io:fwrite("."),
            %erlang:display(First),
			if First == 27 ->
            	if length(NewData) < 3 -> 
    				processInput(InputReceiver, NewData)
                end;
                true ->
                	processInput(InputReceiver, "")
            end
    end,
    processInput(InputReceiver, "").
