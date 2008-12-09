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
-export([start/1]).

-define(WIDTH, 10).
-define(HEIGHT, 10).
-define(APPLE_CNT, 5).

%%
%% API Functions
%%
start(Terminal) ->
    Board = initBoard(),
    Snake = initSnake(),
    Direction = up,
    eventLoop(Board, Snake, Direction, Terminal).


%%
%% Local Functions
%%
initBoard() ->
    Row = lists:duplicate(?WIDTH, free),
    TmpBoard = lists:duplicate(?HEIGHT, Row),
    TmpBoardWithApples = initApples(TmpBoard, ?APPLE_CNT),
    changeField(TmpBoardWithApples, {?WIDTH div 2, ?HEIGHT div 2}, snake).

initSnake() ->
    [{?WIDTH div 2, ?HEIGHT div 2}].

initApples(Board, 0) ->
    Board;
initApples(Board, AppleCnt) ->
    {_, NewBoard} = initApple(Board),
    initApples(NewBoard, AppleCnt - 1).

initApple(Board) ->
    Pos = randomPos(),
    FieldValue = atPosition(Pos, Board),	%% no function calls in this special form (if)...., buh...
    if FieldValue == free ->
           NewBoard = changeField(Board, Pos, apple);
       true ->
           NewBoard = initApple(Board)
    end,
    {Pos, NewBoard}.

randomPos() -> 
    X = random:uniform(?WIDTH),
    Y = random:uniform(?HEIGHT),
    {X, Y}.

    
eventLoop(Board, Snake, Direction, Terminal) ->
    receive
        {quit} ->
            unimplemented;
        {NewDirection} ->
            processStep(Board, Snake, NewDirection, Terminal)
    after 500 ->
    	processStep(Board, Snake, Direction, Terminal)
    end.

processStep(Board, Snake, Direction, Terminal) ->
	NewPos = newPosition(Snake, Direction),
	case atPosition(NewPos, Board) of
		apple ->
			{NewSnake, TmpBoard} = addNewHead(NewPos, Snake, Board, Terminal),
            NewBoard = addApple(TmpBoard, Terminal),
            eventLoop(NewBoard, NewSnake, Direction, Terminal);
		snake ->
			quitGame();
		free ->
            {TmpSnake, TmpBoard} = addNewHead(NewPos, Snake, Board, Terminal),
            {NewSnake, NewBoard} = removeTail(TmpSnake, TmpBoard, Terminal),
            eventLoop(NewBoard, NewSnake, Direction, Terminal)
	end.

addNewHead(Pos, Snake, Board, Terminal) ->
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
    
removeTail(Snake, Board, Terminal) ->
    Pos = lists:last(Snake),
    NewSnake = lists:delete(Pos, Snake),
    Terminal ! {free, Pos},
    {NewSnake, changeField(Board, Pos, free)}.

addApple(Board, Terminal) ->
    {Pos, NewBoard} = initApple(Board), 
    Terminal ! {apple, Pos},
    NewBoard.

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

