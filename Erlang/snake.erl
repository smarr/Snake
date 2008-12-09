%% Author: smarr
%% Created: Dec 9, 2008
%% Description: TODO: Add description to snake
-module(snake).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start() ->
    unimplemented.


%%
%% Local Functions
%%
eventLoop([{X, Y}|SnakeTail], Board) ->
    receive
        {moveLeft} ->
            % Board ! {checkSnake, X - 1, Y}, %% dont do this, it is already done, the board has send you the moveLeft 
            Board ! {remove, }
            
    end.
