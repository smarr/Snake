
%% Define constants used in the snake program
-define(WIDTH, 10).
-define(HEIGHT, 10).
-define(APPLE_CNT, 5).

%% Define some basic data types
-record(board, {height=1, width=1, board=[[free]]}).