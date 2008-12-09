%% Author: smarr
%% Created: Dec 8, 2008
%% Description: TODO: Add description to nestfct
-module(nestfct).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%
test() ->
	R = '34',
	T = fun() ->
		 io:fwrite(R)
	end,
	T().


%%
%% Local Functions
%%

