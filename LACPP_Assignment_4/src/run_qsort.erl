%% File: run_qsort.erl

-module(run_qsort).
-export([run/0, run/1]).

run() ->
	test_qsort_numeric({qsort, qsort1}, 5000).

run(ListSize) ->
	test_qsort_numeric({qsort, qsort1}, ListSize).
	%%halt().
	
test_qsort_numeric({M, F}, ListSize) ->
	List_unsorted = [random:uniform(12345) || _ <- lists:seq(1, ListSize)],
	test_sort({M, F}, "numeric", List_unsorted).
	
test_sort({M, F}, Sort_type, List_unsorted_) ->
	Fun_name = erlang:atom_to_list(F),
	Mod_name = erlang:atom_to_list(M),
	%%List_sorted = M:F(List_unsorted_),
	io:format("~s:~s - ~s.~n", [Mod_name, Fun_name, Sort_type]),
	%%io:format("~w~n", [List_sorted]).
	timer:tc(fun() -> M:F(List_unsorted_) end).