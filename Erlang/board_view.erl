%% Author: smarr
%% Created: Dec 8, 2008
%% Description: TODO: Add description to board_view
-module(board_view).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([update_completely/2,
         clean_field/2,
         put_field/2,
         draw_border/3]).

%%
%% API Functions
%%
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

draw_border(Height, Width, TermServ) ->
    draw_rows(Height, Width, TermServ, true).

%%
%% Local Functions
%%

%% TODO: think about this, currently i use accumulators to know the position, but this could be encoded in the apples/snake as well, depends on the rest of the business logic what is better
update_rows([Head|Tail], TermServ, X, Y) ->
    update_items(Head, TermServ, X, Y),
    update_rows(Tail, TermServ, X, Y + 1);

update_rows([], TermServ, X, Y) ->
    null.


update_items([Head|Tail], TermServ, X, Y) ->
    if Head == apple ->
    	terminal:set_cursor(X + 1, Y + 1, TermServ), %% the +1 is caused by the border
    	terminal:put('o', TermServ)
    end,
    update_items(Tail, TermServ, X + 1, Y);

update_items([], TermServ, X, Y) ->
	null.

draw_rows(Height, Width, TermServ, true) ->
    terminal:set_cursor(1, 1, TermServ),
    terminal:put('/', TermServ),
    draw_col(Width, TermServ, '-'),
    terminal:put('\\~n', TermServ),
    draw_rows(Height, Width, TermServ, false);
draw_rows(Height, Width, TermServ, false) ->
    terminal:put('|', TermServ),
    draw_col(Width, TermServ, ' '),
    terminal:put('|~n', TermServ),
    draw_rows(Height - 1, Width, TermServ, false);
draw_rows(0, Width, TermServ, false) ->
    terminal:put('\\', TermServ),
    draw_col(Width, TermServ, '-'),
    terminal:put('/', TermServ).

draw_col(0, TermServ, Char) ->
    null;
draw_col(Width, TermServ, Char) ->
    terminal:put(Char, TermServ),
    draw_col(Width - 1, TermServ, Char).
