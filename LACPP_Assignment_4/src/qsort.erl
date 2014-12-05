%% File: qsort.erl

-module(qsort).
-export([qsort1/1, pqsort3/1]).

%% Sequential quicksort (version 1)
qsort1([]) ->
	[];
qsort1([P|Xs]) ->
	qsort1([ X || X <- Xs, X =< P]) 
	++ [P] % Pivot
	++ qsort1([X|| X <- Xs, P < X ]).

%% Parallel quicksort (version 3)
pqsort3(L) -> pqsort3(5, L).

pqsort3(0, L) -> 
	qsort1(L);
pqsort3(_, []) ->
	[];
pqsort3(D, [P|Xs]) ->
	Parent = self(),
	Ref = make_ref(),
	spawn_link(fun() ->
				Gs = [X || X <- Xs, P < X],
				Parent ! {Ref, pqsort3(D-1, Gs)}
			   end),
	pqsort3(D-1, [X || X <- Xs, X =< P]) 
	++ [P]
	++ receive {Ref, Ys} -> Ys end.