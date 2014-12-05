-module(run_mergesort).
-export([run/0, run/1]).

run() ->
	run(5000).

run(ListSize) ->
	test_pmergesort_numeric({mergesort, pmergesort2}, ListSize).
	
test_mergesort_numeric({M, F}, ListSize) ->
	List_unsorted = [random:uniform(12345) || _ <- lists:seq(1, ListSize)],
	test_sort({M, F}, "numeric", List_unsorted).

test_pmergesort_numeric({M, F}, ListSize) ->
	List_unsorted = [random:uniform(12345) || _ <- lists:seq(1, ListSize)],
	test_sort({M, F}, "numeric", List_unsorted).
	
test_sort({M, F}, Sort_type, List_unsorted_) ->
	Fun_name = erlang:atom_to_list(F),
	Mod_name = erlang:atom_to_list(M),
	io:format("~s:~s - ~s.~n", [Mod_name, Fun_name, Sort_type]),
	timer:tc(fun() -> M:F(List_unsorted_) end).