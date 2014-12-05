-module(mergesort).
-export([mergesort/1, pmergesort/1, pmergesort/2, pmergesort2/1, pmergesort2/3]).

split(Ls) ->
	split(Ls, Ls, []).
	
split([], Ls1, Ls2) ->
	{lists:reverse(Ls2) , Ls1};
	
split([_], Ls1, Ls2) ->
	{lists:reverse(Ls2) , Ls1};
	
split([_,_|TT], [Ls1_H | Ls1_T], Ls2) ->
    split(TT, Ls1_T, [Ls1_H | Ls2]).

merge(_, [], Ls2) ->
    Ls2;
merge(_, Ls1, []) ->
    Ls1;
merge(Rel, [H1|T1], [H2|T2]) ->
	case Rel(H1, H2) of
		true ->
			[H1 | merge(Rel, T1, [H2|T2])];
		false ->
			[H2 | merge(Rel, [H1|T1], T2)]
	end.

%% Sequential merge sort
mergesort([]) ->
    [];
mergesort([H]) ->
    [H];
mergesort(Ls) ->
	{Half1, Half2} = split(Ls),
	merge(fun lte/2, mergesort(Half1), mergesort(Half2)).

%% Parallel merge sort
%% ~2x slower than sequential version (8 logical cores, 10000 numbers)
pmergesort(Ls) ->
	pmergesort(Ls, self()).


pmergesort([], Parent) ->
    send_msg([], Parent);
pmergesort([H], Parent) ->
    send_msg([H], Parent);
pmergesort(Ls, Parent) ->
	{Half1, Half2} = split(Ls),
	Pid1 = spawn(mergesort, pmergesort, [Half1, self()]),
	Pid2 = spawn(mergesort, pmergesort, [Half2, self()]),
	LLeft = receive_msg(Pid1),
	LRight = receive_msg(Pid2),
	send_msg(merge(fun lte/2, LLeft, LRight), Parent).

%% Parallel merge sort (with controlled granularity)
%% ~2x faster than sequential version (8 logical cores, 10000 numbers)
pmergesort2(Ls) ->
	pmergesort2(5, Ls, self()).

pmergesort2(_, [], Parent) ->
    send_msg([], Parent);
pmergesort2(_, [H], Parent) ->
    send_msg([H], Parent);
pmergesort2(0, Ls, Parent) ->
    send_msg(mergesort(Ls), Parent);
pmergesort2(D, Ls, Parent) ->
	{Half1, Half2} = split(Ls),
	Pid1 = spawn(mergesort, pmergesort2, [D-1, Half1, self()]),
	Pid2 = spawn(mergesort, pmergesort2, [D-1, Half2, self()]),
	LLeft = receive_msg(Pid1),
	LRight = receive_msg(Pid2),
	send_msg(merge(fun lte/2, LLeft, LRight), Parent).


%% Receive message from a process
receive_msg(FromPid) ->
	receive
		{FromPid, Ls} -> Ls
	end.

%% Send message to a process
send_msg(Ls, ToPid) ->
	ToPid ! {self(), Ls}.
	
%% Lesser than or equal to
lte(X, Y) ->
	(X < Y) or (X == Y).