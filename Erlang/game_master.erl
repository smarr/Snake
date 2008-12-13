%% Author: smarr
%% Created: Dec 9, 2008
%% Description: GameMaster actor implementation
%%   This actor accepts the following messages:
%%     - left, right, up, down, quit
%%   after 500msec, an event is issued automatically to move the snake
-module(game_master).

%%
%% Include files
%% 
-include("definitions.hrl").
-include("/opt/local/lib/erlang/lib/eunit-2.0/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([start/1]).


%%
%% API Functions
%%
start(Terminal) ->
    Board = initBoard(?WIDTH, ?HEIGHT, ?APPLE_CNT),
    Terminal ! {board, Board},
    Snake = initSnake(Board),
    Direction = up,
    eventLoop(Board, Snake, Direction, Terminal).


%%
%% Local Functions
%%
eventLoop(Board, Snake, Direction, Terminal) ->
    receive
        {quit} ->
            unimplemented;
        {NewDirection} ->
            processStep(Board, Snake, NewDirection, Terminal)
    after 500 ->
    	processStep(Board, Snake, Direction, Terminal)
    end.

initBoard(Width, Height, AppleCnt) ->
    TmpBoard = buildBoard(Width, Height),
    TmpBoardWithSnake = changeField(TmpBoard, {1 + Width div 2, 1 + Height div 2}, snake),
    initApples(TmpBoardWithSnake, AppleCnt).

buildBoard(Width, Height) ->
    Row = lists:duplicate(Width, free),
    BoardData = lists:duplicate(Height, Row),
	#board{height=Height, width=Width, board=BoardData}.

buildBoard_test() ->
    #board{height=3, width=3, 
           board=[[free, free, free],
     			  [free, free, free],
     			  [free, free, free]]} = buildBoard(3, 3).

initBoard_test() ->
    #board{height=3, width=3, 
           board=[[_, _, _],
     			  [_, snake, _],
     			  [_, _, _]]} = initBoard(3, 3, 1).

initSnake(Board) ->
    [{1 + Board#board.width div 2, 1 + Board#board.height div 2}].

initSnake_test() ->
    Board = #board{height=9, width=9},
    [{5,5}] = initSnake(Board).

initApples(Board, 0) ->
    Board;
initApples(Board, AppleCnt) ->
    {_, NewBoard} = initApple(Board),
    initApples(NewBoard, AppleCnt - 1).

initApple(Board) ->
    Pos = randomPos(Board#board.height, Board#board.width),
    erlang:display(Pos),
    FieldValue = atPosition(Pos, Board),	%% no function calls in this special form (if)...., buh...
    if FieldValue == free ->
           NewBoard = changeField(Board, Pos, apple);
       true ->
           NewBoard = initApple(Board)
    end,
    {Pos, NewBoard}.

randomPos(Height, Width) -> 
    X = random:uniform(Width),
    Y = random:uniform(Height),
    {X, Y}.

processStep(Board, Snake, Direction, Terminal) ->
	NewPos = newPosition(Snake, Direction, Board),
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
    {Rows, RowsBelow} = lists:split(Y, Board#board.board),
    {RowsAbove, [Row]} = lists:split(Y - 1, Rows),
    {Fields, FieldsRight} = lists:split(X, Row),
    {FieldsLeft, _} = lists:split(X - 1, Fields),
    Board#board{board=RowsAbove ++ [FieldsLeft ++ [NewValue] ++ FieldsRight] ++ RowsBelow}.

changeField_test() ->
    Board = #board{height=3, width=3, board=[[free, free, free],
    					 			   [free, free, free],
     			  					   [free, free, free]]},
    #board{height=3, width=3, board=[[free, free, free],
    					 			   [free, free, free],
     			  					   [apple, free, free]]}
			= changeField(Board, {1, 3}, apple).
    
removeTail(Snake, Board, Terminal) ->
    Pos = lists:last(Snake),
    NewSnake = lists:delete(Pos, Snake),
    Terminal ! {free, Pos},
    {NewSnake, changeField(Board, Pos, free)}.

addApple(Board, Terminal) ->
    {Pos, NewBoard} = initApple(Board), 
    Terminal ! {apple, Pos},
    NewBoard.

newPosition([{X, Y}|_], Direction, Board) ->
    case Direction of
        left  -> overflow({X - 1, Y}, Board);
        right -> overflow({X + 1, Y}, Board);
        up    -> overflow({X, Y - 1}, Board);
        down  -> overflow({X, Y + 1}, Board)
    end.

overflow({X, Y}, Board) ->
    {overflow(X, Board#board.width), overflow(Y, Board#board.height)};
overflow(Val, Max) ->
    if	Val < 0 ->
           Val rem Max + Val;
    	true ->
           Val rem Max
    end.

atPosition({X, Y}, Board) ->
    Row = lists:nth(Y, Board#board.board),
    lists:nth(X, Row).

atPosition_test() ->
	free = atPosition({2, 1}, #board{height=2, width=2, board=
                                         [[apple, free],
                               			  [apple, apple]]}).
    
quitGame() ->
    io:fwrite("GAME OVER"),
	halt().

