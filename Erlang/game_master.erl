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
-export([start/0, quit/0, set_input/2]).


%%
%% API Functions
%%
start() ->
    Board = initBoard(?WIDTH, ?HEIGHT, ?APPLE_CNT),
    board_view:display_board(Board),
    Snake = initSnake(Board),
    Direction = up,
    receive
        {set_input, Sender} ->
            eventLoop(Board, Snake, Direction, Sender)
    end.

set_input(GameMaster, Sender) ->
    GameMaster ! {set_input, Sender}.


quit() ->
    halt().


%%
%% Local Functions
%%
eventLoop(Board, Snake, Direction, Sender) ->
    timer:sleep(500),
    receive
		Message ->
			case receive_last(Message) of
        		{_Sender, left} ->
					processStep(Board, Snake, Sender, left);
        		{_Sender, right} ->
            		processStep(Board, Snake, Sender, right);
        		{_Sender, up} ->
            		processStep(Board, Snake, Sender, up);
        		{_Sender, down} ->
            		processStep(Board, Snake, Sender, down);
        		Other -> % Flushes the message queue. 
					error_logger:error_msg( 
						"[Game Master] Error: Process ~w got unknown msg ~w~n.", 
						[self(), Other]),
            			processStep(Board, Snake, Sender, Direction)
           end
    after 0 ->
    	processStep(Board, Snake, Sender, Direction)
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
    TmpBoardWithSnake = board:changeField(TmpBoard, {1 + Width div 2, 1 + Height div 2}, snake),
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
    FieldValue = board:atPosition(Pos, Board),	%% no function calls in this special form (if)...., buh...
    if FieldValue == free ->
           NewBoard = board:changeField(Board, Pos, apple),
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
    board_view:debug({21, 19}, pid_to_list(Sender)),
	NewPos = newPosition(Snake, Direction, Board),
	case board:atPosition(NewPos, Board) of
		apple ->
			{NewSnake, TmpBoard} = addNewHead(NewPos, Snake, Board),
            NewBoard = addApple(TmpBoard),
           	snake:return_data(Sender, {NewBoard, NewSnake, Direction}),
            eventLoop(NewBoard, NewSnake, Direction, Sender);
		snake ->
			quitGame();
		free ->
            {TmpSnake, TmpBoard} = addNewHead(NewPos, Snake, Board),
            {NewSnake, NewBoard} = removeTail(TmpSnake, TmpBoard),
           	snake:return_data(Sender, {NewBoard, NewSnake, Direction}),
            eventLoop(NewBoard, NewSnake, Direction, Sender)
	end.

addNewHead(Pos, Snake, Board) ->
    NewSnake = [Pos] ++ Snake,
    board_view:show_snake_head(Pos),
    {NewSnake, board:changeField(Board, Pos, snake)}.
      
removeTail(Snake, Board) ->
    Pos = lists:last(Snake),
    NewSnake = lists:delete(Pos, Snake),
    board_view:free(Pos),
    {NewSnake, board:changeField(Board, Pos, free)}.

addApple(Board) ->
    {Pos, NewBoard} = initApple(Board), 
    board_view:show_apple(Pos),
    NewBoard.

newPosition([{X, Y}|_], Direction, Board) ->
    case Direction of
        left  -> board:overflow({X - 1, Y}, Board);
        right -> board:overflow({X + 1, Y}, Board);
        up    -> board:overflow({X, Y - 1}, Board);
        down  -> board:overflow({X, Y + 1}, Board)
    end.
    
quitGame() ->
    io:fwrite("GAME OVER"),
	halt().

