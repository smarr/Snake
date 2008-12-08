%% Author: smarr
%% Created: Dec 8, 2008
%% Description: Terminal process.
%%   - receives data from the connected server process
%%	   this connection must be established outside of the module, 
%%     and the resulting process id must be given to the server
%%   - sends events about input to the receiver 
-module(terminal).

%%
%% Exported Functions
%%
-export([start/1, 
         set_cursor/3, 
         clear/1, 
         put/2]).

%%
%% API Functions
%%
start(InputReceiver) -> 
    processInput(InputReceiver).

set_cursor(X, Y, {Terminal, Server}) ->
    Server ! {Terminal, {command, "[" + Y + ";" + X + "H"}}.

clear({Terminal, Server}) ->
    Server ! {Terminal, {command, "[2J"}}.

put(String, {Terminal, Server}) ->
    Server ! {Terminal, {command, String}}.

%%
%% Local Functions
%%
processInput(InputReceiver) ->
    receive
    {Server, {data, Bytes}} -> 
        case Bytes of 
            '[A' -> 
                InputReceiver ! {key_up};
            '[B' ->
                InputReceiver ! {key_down};
            '[C' ->
                InputReceiver ! {key_right};
            '[D' ->
                InputReceiver ! {key_left}
            %% TODO: propably, an exit clause will be needed here
        end
    after 500 ->
            InputReceiver ! {heartbeat}
    end,
	processInput(InputReceiver).
