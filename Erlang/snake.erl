%% Author: smarr
%% Created: Jan 21, 2009
%% Description: TODO: Add description to snake
-module(snake).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([move_left/1,
         move_right/1,
         move_up/1,
         move_down/1,
         return_data/2]).

%%
%% API Functions
%%

%% Output for stearing
move_left(Receiver) ->
	Receiver ! {self(), left}.

move_right(Receiver) ->
    Receiver ! {self(), right}.

move_up(Receiver) ->
    Receiver ! {self(), up}.

move_down(Receiver) ->
    Receiver ! {self(), down}.

%% Input for next stearing step
return_data(Receiver, {Board, Snake}) ->
    Receiver ! {self(), Board, Snake}.

%%
%% Local Functions
%%

