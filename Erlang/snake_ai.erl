%% Author: smarr
%% Created: Jan 21, 2009
%% Description: TODO: Add description to snake_ai
-module(snake_ai).

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
start(GameMaster) ->
    board_view:debug({20, 20}, "X"),
    board_view:debug({21, 20}, pid_to_list(self())),
    processStatusMessages(GameMaster, 0).


%%
%% Local Functions
%%
processStatusMessages(GameMaster, Count) ->
    receive
        % here Sender == GameMaster should be true
        {Sender, Board, Snake, Direction} ->
            [{X, Y}|_Tail] = Snake,
            %% simple strategy, go for an apple if it is next to you
            % left, right, up, down
            ValLeft = board:atPosition(board:overflow({X - 1, Y}, Board), Board),
            if ValLeft == apple ->
            	  snake:move_left(GameMaster);
               true ->
                  ValRight = board:atPosition(board:overflow({X + 1, Y}, Board), Board),
                  if ValRight == apple ->
                  		snake:move_right(GameMaster);
                     true ->
                        ValUp = board:atPosition(board:overflow({X, Y - 1}, Board), Board),
                        if ValUp == apple ->
                               snake:move_up(GameMaster);
                           true ->
                               ValDown = board:atPosition(board:overflow({X, Y + 1}, Board), Board),
                               if ValDown == down ->
                                      snake:move_down(GameMaster);
                                  true ->
                                      do_random_stuff(ValLeft, ValRight, ValUp, ValDown, Direction, GameMaster)
                               end
                        end
                  end
            end;
    	Other -> % Flushes the message queue. 
					error_logger:error_msg( 
						"[SnakeAI] Error: Process ~w got unknown msg ~w~n.", 
						[self(), Other])
    end,
    board_view:debug({20 + Count, 20}, "X"),
    processStatusMessages(GameMaster, Count + 1).

do_random_stuff(ValLeft, ValRight, ValUp, ValDown, Direction, GameMaster) ->
    case {random:uniform(33), ValLeft, ValRight, ValUp, ValDown, Direction} of
		{1, free, _, _, _, _} -> snake:move_left(GameMaster);
        {2, _, free, _, _, _} -> snake:move_right(GameMaster);
        {3, _, _, free, _, _} -> snake:move_up(GameMaster);
        {4, _, _, _, free, _} -> snake:move_down(GameMaster);
        {_Else, free, _, _, _, left} -> snake:move_direction(GameMaster, Direction);
        {_Else, _, free, _, _, right} -> snake:move_direction(GameMaster, Direction);
        {_Else, _, _, free, _, up} -> snake:move_direction(GameMaster, Direction);
        {_Else, _, _, _, free, down} -> snake:move_direction(GameMaster, Direction);
    	_Else ->
            do_random_stuff(ValLeft, ValRight, ValUp, ValDown, Direction, GameMaster)
	end.
