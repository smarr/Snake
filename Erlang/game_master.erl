%% Author: smarr
%% Created: Dec 9, 2008
%% Description: TODO: Add description to game_master
-module(game_master).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

-define(WIDTH, 10).
-define(HEIGHT, 10).

%%
%% API Functions
%%
start() ->
    eventLoop(unimplemented, unimplemented, unimplemented),
    unimplemented.


%%
%% Local Functions
%%
eventLoop(Board, Snake, Direction) ->
    receive
        {quit} ->
            unimplemented;
        {NewDirection} ->
            processStep(Board, Snake, NewDirection)
    after 500 ->
    	processStep(Board, Snake, Direction)
    end.

processStep(Board, Snake, Direction) ->
	NewPos = newPosition(Snake, Direction),
	case atPosition(NewPos, Board) of
		apple ->
			{NewSnake, TmpBoard} = addNewHead(NewPos, Snake, Board),
            NewBoard = addApple(TmpBoard),
            eventLoop(NewBoard, NewSnake, Direction);
		snake ->
			quitGame();
		free ->
            {TmpSnake, TmpBoard} = addNewHead(NewPos, Snake, Board),
            {NewSnake, NewBoard} = removeTail(TmpSnake, TmpBoard),
            eventLoop(NewBoard, NewSnake, Direction)
	end.

addNewHead(Pos, Snake, Board) ->
    NewSnake = [Pos] ++ Snake,
    Terminal ! {snake, Pos},
    {NewSnake, changeField(Board, Pos, snake)}.
    
changeField(Board, Pos, NewValue) ->
	{X, Y} = Pos,
    {Rows, RowsBelow} = lists:split(Y, Board),
    {RowsAbove, Row} = lists:split(Y - 1, Rows),
    {Fields, FieldsRight} = lists:split(X, Row),
    {FieldsLeft, _} = lists:split(X - 1, Fields),
    RowsAbove ++ [FieldsLeft ++ [NewValue] ++ FieldsRight] ++ RowsBelow.
    
removeTail(Snake, Board) ->
    Pos = lists:last(Snake),
    NewSnake = lists:delete(Pos, Snake),
    Terminal ! {free, Pos},
    {NewSnake, changeField(Board, Pos, free)}.

addApple(Board) ->
    Terminal ! {apple, Pos},
    unimplemented.

newPosition([{X, Y}|_], Direction) ->
    case Direction of
        left  -> overflow({X - 1, Y});
        right -> overflow({X + 1, Y});
        up    -> overflow({X, Y - 1});
        down  -> overflow({X, Y + 1})
    end.

overflow({X, Y}) ->
    {overflow(X, ?WIDTH), overflow(Y, ?HEIGHT)}.

overflow(Val, Max) ->
    if	Val < 0 ->
           Val rem Max + Val;
    	true ->
           Val rem Max
    end.

atPosition({X, Y}, Board) ->
    Row = lists:nth(Y, Board),
    lists:nth(X, Row).
    
quitGame() ->
    io:fwrite("GAME OVER"),
	halt().

