%% Author: smarr
%% Created: Dec 9, 2008
%% Description: GameMaster actor implementation
%%   This actor accepts the following messages:
%%     - left, right, up, down, quit
%%   after 500msec, an event is issued automatically to move the snake
%% implements the message protocol defined in snake.erl and uses input function to give feedback
-module(game_master).

%%
%% Include files
%% 
-include("definitions.hrl").
-include("/opt/local/lib/erlang/lib/eunit-2.0/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([start/0]).


%%
%% API Functions
%%
start() ->
    Board = initBoard(?WIDTH, ?HEIGHT, ?APPLE_CNT),
    board_view:display_board(Board),
    Snake = initSnake(Board),
    Direction = up,
    eventLoop(Board, Snake, Direction).

quit() ->
    halt().


%%
%% Local Functions
%%
eventLoop(Board, Snake, Direction) ->
    timer:sleep(500),
    receive
		Message ->
			case receive_last(Message) of
        		{Sender, left} ->
					processStep(Board, Snake, Sender, left);
        		{Sender, right} ->
            		processStep(Board, Snake, Sender, right);
        		{Sender, up} ->
            		processStep(Board, Snake, Sender, up);
        		{Sender, down} ->
            		processStep(Board, Snake, Sender, down);
        		Other -> % Flushes the message queue. 
					error_logger:error_msg( 
						"[Game Master] Error: Process ~w got unknown msg ~w~n.", 
						[self(), Other]),
            			processStep(Board, Snake, self(), Direction)
           end
    after 0 ->
    	processStep(Board, Snake, self(), Direction)
    end.

receive_last(Last) ->
    receive
        AnyMessage ->
            receive_last(AnyMessage)
    	after 0 ->
            Last
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
    FieldValue = atPosition(Pos, Board),	%% no function calls in this special form (if)...., buh...
    if FieldValue == free ->
           NewBoard = changeField(Board, Pos, apple),
           {Pos, NewBoard};
       true ->
           initApple(Board)
    end.

initApple_test() ->
    Board = #board{height=3, width=3, board=[[snake, snake, snake],
                                             [snake, snake, snake],
                                             [free, snake, snake]]},
    {{1, 3}, #board{height=3, width=3, board=[[snake, snake, snake],
                                             [snake, snake, snake],
                                             [apple, snake, snake]]}}
                   = initApple(Board).

randomPos(Height, Width) -> 
    X = random:uniform(Width),
    Y = random:uniform(Height),
    {X, Y}.

processStep(Board, Snake, Sender, Direction) ->
	NewPos = newPosition(Snake, Direction, Board),
	case atPosition(NewPos, Board) of
		apple ->
			{NewSnake, TmpBoard} = addNewHead(NewPos, Snake, Board),
            NewBoard = addApple(TmpBoard),
            if Sender /= self() ->
            	snake:return_data(Sender, {NewBoard, NewSnake});
           	  true -> null
            end, 
            eventLoop(NewBoard, NewSnake, Direction);
		snake ->
			quitGame();
		free ->
            {TmpSnake, TmpBoard} = addNewHead(NewPos, Snake, Board),
            {NewSnake, NewBoard} = removeTail(TmpSnake, TmpBoard),
            if Sender /= self() ->
            	snake:return_data(Sender, {NewBoard, NewSnake});
              true -> null
            end,
            eventLoop(NewBoard, NewSnake, Direction)
	end.

addNewHead(Pos, Snake, Board) ->
    NewSnake = [Pos] ++ Snake,
    board_view:show_snake_head(Pos),
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
    
removeTail(Snake, Board) ->
    Pos = lists:last(Snake),
    NewSnake = lists:delete(Pos, Snake),
    board_view:free(Pos),
    {NewSnake, changeField(Board, Pos, free)}.

addApple(Board) ->
    {Pos, NewBoard} = initApple(Board), 
    board_view:show_apple(Pos),
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
    ValMinOne = Val - 1,
    if	ValMinOne < 0 ->
           TmpRes = ValMinOne rem Max + Max;
    	true ->
           TmpRes = ValMinOne rem Max
    end,
    TmpRes + 1.

overflow_test() ->
    ?assertMatch(10, overflow(0, 10)),
    ?assertMatch( 1, overflow(11, 10)),
    ?assertMatch( 1, overflow(1, 10)).

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

