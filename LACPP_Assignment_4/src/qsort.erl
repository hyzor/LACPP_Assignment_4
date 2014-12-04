%% File: qsort.erl

-module(qsort).
-export([qsort1/1]).

qsort1([]) ->
	[];
qsort1([H | T]) ->
	qsort1([ X || X <- T, X < H]) ++ [H] ++ qsort1([ X || X <- T, X >= H ]).