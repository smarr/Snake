%% Author: smarr
%% Created: Jan 20, 2009
%% Description: TODO: Add description to game
-module(game).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%

start(human) ->
    GameMaster = spawn_link(game_master, start, []),
    spawn_link(terminal_reader, start, [GameMaster]);
start(ai) ->
    GameMaster = spawn_link(game_master, start, []),
    spawn_link(snake_ai, start, [GameMaster]).

%%
%% Local Functions
%%

