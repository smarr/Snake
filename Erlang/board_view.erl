%% Author: smarr
%% Created: Dec 8, 2008
%% Description: TODO: Add description to board_view
-module(board_view).

%%
%% Include files
%%
-include("definitions.hrl").

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start() ->
    terminal:clean(),
    draw_border(?HEIGHT, ?WIDTH),
	eventLoop().

%%
%% Local Functions
%%
eventLoop() ->
    receive
        {board, Board} -> 
            update_completely(Board);
        {snake, {X, Y}} ->
            put_field(snake, X, Y);
        {apple, {X, Y}} ->
            put_field(apple, X, Y);
        {free, {X, Y}} ->
            clean_field(X, Y);
        SomeThing -> erlang:display(SomeThing)
    end,
    eventLoop().

update_completely(Board) ->
    update_rows(Board#board.board, 1, 1).

clean_field(X, Y) ->
    terminal:set_cursor(X + 1, Y + 1), %% the +1 is caused by the border
    terminal:put(' ').
	  
put_field(apple, X, Y) ->
    terminal:set_cursor(X + 1, Y + 1), %% the +1 is caused by the border
    terminal:put('o');
put_field(snake, X, Y) ->
    terminal:set_cursor(X + 1, Y + 1), %% the +1 is caused by the border
    terminal:put('#').

draw_border(Height, Width) ->
    draw_rows(Height, Width, true).

%% TODO: think about this, currently i use accumulators to know the position, but this could be encoded in the apples/snake as well, depends on the rest of the business logic what is better
update_rows([Head|Tail], X, Y) ->
    update_items(Head, X, Y),
    update_rows(Tail, X, Y + 1);

update_rows([], _, _) ->
    null.


update_items([Head|Tail], X, Y) ->
    %erlang:display({Head, Tail, X, Y}),
    if Head == apple ->
			terminal:set_cursor(X + 1, Y + 1), %% the +1 is caused by the border
    		terminal:put('o');
       Head == snake ->
			terminal:set_cursor(X + 1, Y + 1), %% the +1 is caused by the border
			terminal:put('#');
       true -> true
    end,
    update_items(Tail, X + 1, Y);

update_items([], _, _) ->
	null.

draw_rows(0, Width, false) ->
    terminal:put('\\'),
    draw_col(Width, '-'),
    terminal:put('/');
draw_rows(Height, Width, true) ->
    terminal:set_cursor(1, 1),
    terminal:put('/'),
    draw_col(Width, '-'),
    terminal:put('\\~n'),
    draw_rows(Height, Width, false);
draw_rows(Height, Width, false) ->
    terminal:put('|'),
    draw_col(Width, ' '),
    terminal:put('|~n'),
    draw_rows(Height - 1, Width, false).


draw_col(0, _) ->
    null;
draw_col(Width, Char) ->
    terminal:put(Char),
    draw_col(Width - 1, Char).
