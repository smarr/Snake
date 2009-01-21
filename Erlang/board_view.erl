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
-export([start/0,
         init/0,
         display_board/1,
         show_snake_head/1,
         show_apple/1,
         free/1,
         debug/2]).

%%
%% API Functions
%%
init() ->
    register(board_view, self()).

start() ->
    terminal:clean(),
    eventLoop(dict:new()).

display_board(Board) ->
    board_view ! {board, Board, self()}.

show_snake_head(Pos) ->
    board_view ! {snake, Pos, self()}.

show_apple(Pos) ->
    board_view ! {apple, Pos, self()}.
  
free(Pos) ->
    board_view ! {free, Pos, self()}.

debug(Pos, Message) ->
    board_view ! {debug, Pos, self(), Message}.


%Display ! {board, Board},

%%
%% Local Functions
%%
eventLoop(Dict) ->
    receive
        {board, Board, Sender} ->
            {NewDict, Offset} = updateRegisteredViews(Sender, Dict),
            update_completely(Board, Offset);
        {snake, {X, Y}, Sender} ->
            {NewDict, Offset} = updateRegisteredViews(Sender, Dict),
            put_field(snake, X, Y, Offset);
        {apple, {X, Y}, Sender} ->
            {NewDict, Offset} = updateRegisteredViews(Sender, Dict),
            put_field(apple, X, Y, Offset);
        {free, {X, Y}, Sender} ->
            {NewDict, Offset} = updateRegisteredViews(Sender, Dict),
            clean_field(X, Y, Offset);
        {debug, {X, Y}, Sender, Message} ->
            NewDict = Dict,
			terminal:set_cursor(X + 1, Y + 1), %% the +1 is caused by the border
    		terminal:put(Message);
        SomeThing ->
            erlang:display(SomeThing),
            NewDict = Dict   
    end,
    eventLoop(NewDict).

updateRegisteredViews(Sender, Dict) ->
    IsInDict = dict:is_key(Sender, Dict),
    if IsInDict ->
    	   Offset = dict:fetch(Sender, Dict),
           {Dict, Offset};
       true ->
           Offset = newOffset(dict:size(Dict)),
           {dict:store(Sender, Offset, Dict), Offset}
    end.

newOffset(Index) ->
    Y = ((Index * 12) div (12 * 5)) * 12,
    X = (Index * 12) rem (12 * 5),
    {X, Y}.

update_completely(Board, Offset) ->
    draw_border(?HEIGHT, ?WIDTH, Offset),
    update_rows(Board#board.board, 1, 1, Offset).

clean_field(X, Y, {OffX, OffY}) ->
    terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY), %% the +1 is caused by the border
    terminal:put(' ').
	  
put_field(apple, X, Y, {OffX, OffY}) ->
    terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY), %% the +1 is caused by the border
    terminal:put('o');
put_field(snake, X, Y, {OffX, OffY}) ->
    terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY), %% the +1 is caused by the border
    terminal:put('#').

draw_border(Height, Width, Offset) ->
    draw_rows(Height, Width, Offset, true).

%% TODO: think about this, currently i use accumulators to know the position, 
%% but this could be encoded in the apples/snake as well, depends on the rest of the business logic what is better
update_rows([Head|Tail], X, Y, Offset) ->
    update_items(Head, X, Y, Offset),
    update_rows(Tail, X, Y + 1, Offset);

update_rows([], _, _, _) ->
    null.


update_items([Head|Tail], X, Y, {OffX, OffY}) ->
    %erlang:display({Head, Tail, X, Y}),
    if Head == apple ->
			terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY), %% the +1 is caused by the border
    		terminal:put('o');
       Head == snake ->
			terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY), %% the +1 is caused by the border
			terminal:put('#');
       true -> true
    end,
    update_items(Tail, X + 1, Y, {OffX, OffY});

update_items([], _, _, _) ->
	null.

draw_rows(0, Width, {OffX, OffY}, false) ->
    terminal:move_forward(OffX),
    terminal:put('\\'),
    draw_col(Width, '-'),
    terminal:put('/');
draw_rows(Height, Width, {OffX, OffY}, true) ->
    terminal:set_cursor(1 + OffX, 1 + OffY),
    terminal:put('/'),
    draw_col(Width, '-'),
    terminal:put('\\~n'),
    draw_rows(Height, Width, {OffX, OffY}, false);
draw_rows(Height, Width, {OffX, OffY}, false) ->
    terminal:move_forward(OffX),
    terminal:put('|'),
    draw_col(Width, ' '),
    terminal:put('|~n'),
    draw_rows(Height - 1, Width, {OffX, OffY}, false).


draw_col(0, _) ->
    null;
draw_col(Width, Char) ->
    terminal:put(Char),
    draw_col(Width - 1, Char).
