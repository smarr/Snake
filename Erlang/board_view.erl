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
        SomeThing -> erlang:display(SomeThing)
    end,
    eventLoop().

update_completely(Board, TermServ) ->
    update_rows(Board, TermServ, 1, 1).

clean_field({X, Y}, TermServ) ->
    terminal:set_cursor(X + 1, Y + 1, TermServ), %% the +1 is caused by the border
    terminal:put(' ', TermServ).
	  
put_field({apple, X, Y}, TermServ) ->
    terminal:set_cursor(X + 1, Y + 1, TermServ), %% the +1 is caused by the border
    terminal:put('o', TermServ);
put_field({snake, X, Y}, TermServ) ->
    terminal:set_cursor(X + 1, Y + 1, TermServ), %% the +1 is caused by the border
    terminal:put('#', TermServ).

draw_border(Height, Width) ->
    draw_rows(Height, Width, true).

%% TODO: think about this, currently i use accumulators to know the position, but this could be encoded in the apples/snake as well, depends on the rest of the business logic what is better
update_rows([Head|Tail], TermServ, X, Y) ->
    update_items(Head, TermServ, X, Y),
    update_rows(Tail, TermServ, X, Y + 1);

update_rows([], _, _, _) ->
    null.


update_items([Head|Tail], TermServ, X, Y) ->
    if Head == apple ->
    	terminal:set_cursor(X + 1, Y + 1, TermServ), %% the +1 is caused by the border
    	terminal:put('o', TermServ)
    end,
    update_items(Tail, TermServ, X + 1, Y);

update_items([], _, _, _) ->
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
