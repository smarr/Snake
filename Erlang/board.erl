%% Author: smarr
%% Created: Jan 21, 2009
%% Description: TODO: Add description to board
-module(board).

%%
%% Include files
%%
-include("definitions.hrl").
-include("/opt/local/lib/erlang/lib/eunit-2.0/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([atPosition/2,
         overflow/2,
         changeField/3]).

%%
%% API Functions
%%

atPosition({X, Y}, Board) ->
    Row = lists:nth(Y, Board#board.board),
    lists:nth(X, Row).

atPosition_test() ->
	free = atPosition({2, 1}, #board{height=2, width=2, board=
                                         [[apple, free],
                               			  [apple, apple]]}).

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

%%
%% Local Functions
%%

