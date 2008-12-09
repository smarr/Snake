%% Author: smarr
%% Created: Dec 8, 2008
%% Description: TODO: Add description to demo
-module(demo).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([double/1]).
-export([start/0]).
-export([tttt/1, tt/0]).

%%
%% API Functions
%%
double(X) ->
	times(X, 2).

start() ->
    Pid = getty:start(7788, {demo, tttt}).

tttt(Server) ->
    Self = self(),
    Server ! {Self, {command, "[2Jtest Bytes"}},
    io:put_chars(" testttt "),
    io:fwrite("  test juhu :)~n").

tt() ->
	io:fwrite("[2J").

%%
%% Local Functions
%%
times(X, N) ->
	X * N.
