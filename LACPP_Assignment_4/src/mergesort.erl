-module(mergesort).
-export([msort/1]).

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

msort([]) ->
    [];
msort([H]) ->
    [H];
msort(Ls) ->
	{Half1, Half2} = split(Ls),
	merge(fun lte/2, msort(Half1), msort(Half2)).
	
	
lte(X, Y) ->
	(X < Y) or (X == Y).