%% -*- erlang-indent-level: 2 -*-
%% -------------------------------------------------------------------
%% This code comes from an Erlang program, originally written by John
%% Hughes, to solve the Sudoku puzzle and be used as a laboratory
%% exercise for his 2014 Parallel Functional Programming course at
%% Chalmers.
%%
%% It has been cleaned up a bit and modified by Kostis Sagonas who is
%% thus responsible for any bug or problem that might exist.
%% -------------------------------------------------------------------
-module(sudoku).

-export([benchmarks/0, par_benchmarks/0, benchmark/0]).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.

-type elem()   :: 0..9.
-type matrix() :: [[elem()]].
-type name()   :: atom().
-type puzzle() :: {name(), matrix()}.

-type musecs() :: non_neg_integer().
-type result() :: [{name(), float()}].

%%
%% benchmarking code
%%
-define(EXECUTIONS, 1).
-define(PROBLEMS, "sudoku_problems.txt").

-define(BENCHMARK_TARGET, real_challenge).

-spec benchmarks() -> {musecs(), result()}.
benchmarks() ->
  {ok, Puzzles} = file:consult(?PROBLEMS),
  timer:tc(fun () -> benchmarks(Puzzles) end).

-spec benchmarks([puzzle()]) -> result().
benchmarks(Puzzles) ->
  [{Name, bm(fun() -> solve(M) end)} || {Name, M} <- Puzzles].

-spec benchmark() -> {musecs(), result()}.
benchmark() ->
  {ok, Puzzles} = file:consult(?PROBLEMS),
  timer:tc(fun () -> benchmark(Puzzles) end).

-spec benchmark([puzzle()]) -> result().
benchmark(Puzzles) ->
  [{?BENCHMARK_TARGET, bm(fun() -> solve(M) end)} || {?BENCHMARK_TARGET, M} <- Puzzles].

%% EDIT: Parallel benchmarks
-spec par_benchmarks() -> {musecs(), result()}.
par_benchmarks() ->
  {ok, Puzzles} = file:consult(?PROBLEMS),
  timer:tc(fun () -> par_benchmarks(Puzzles) end).

-spec par_benchmarks([puzzle()]) -> result().
par_benchmarks(Puzzles) ->	
	Parent = self(),
	
	%% Create a link of processes, each process sends a message to Parent containing the value of the benchmark of the
	%% current puzzle for the current process
	Pids = [spawn_link(fun() -> send_msg({Name, bm(fun() -> solve(Val) end)}, Parent) end) || {Name, Val} <- Puzzles],

	%% Now wait for all the processes to finish their tasks (by receiving all their messages)
	[receive_msg(Pid) || Pid <- Pids].
%% EDIT end

%% EDIT: Receive and send functions
%% Receive message from a process
receive_msg(FromPid) ->
	receive
		{FromPid, Msg} -> Msg
	end.

%% Send message to a process
send_msg(Msg, ToPid) ->
	ToPid ! {self(), Msg}.
%% EDIT end

bm(F) ->
  {T, _} = timer:tc(fun () -> repeat(?EXECUTIONS, F) end),
  T / ?EXECUTIONS / 1000.

-spec repeat(non_neg_integer(), fun(() -> term())) -> 'ok'.
repeat(0, _) -> ok;
repeat(N, F) when N > 0 ->
  F(), repeat(N-1, F).

%%
%% solve a Sudoku puzzle
%%
solve(M) ->
  Solution = solve_refined(refine(fill(M))),
  case valid_solution(Solution) of
    true ->
      Solution;
    false ->
      exit({invalid_solution, Solution})
  end.

solve_refined(M) ->
  case solved(M) of
    true ->
      M;
    false ->
      solve_one(guesses(M))
  end.

solve_one([]) ->
  exit(no_solution);
solve_one([M]) ->
  solve_refined(M);
solve_one([M|Ms]) ->
  case catch solve_refined(M) of
    {'EXIT',no_solution} ->
      solve_one(Ms);
    Solution ->
      Solution
  end.

%% is a puzzle solved?

solved(M) ->
  lists:all(fun solved_row/1, M).

solved_row(Row) ->
  lists:all(fun is_filled/1, Row).

is_filled(X) ->
  1 =< X andalso X =< 9.

%% check solutions for validity

valid_solution(M) ->
  valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).

valid_rows(M) ->
  lists:all(fun valid_row/1, M).

-define(NINE, [1, 2, 3, 4, 5, 6, 7, 8, 9]).

valid_row(Row) ->
  lists:usort(Row) =:= ?NINE.

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
  Nine = ?NINE,
  [[case is_filled(X) of true -> X; false -> Nine end || X <- Row] || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
  NewM =
   	par_refine_rows(
      transpose(
	refine_rows(
	  transpose(
	    unblocks(
	      refine_rows(
		blocks(M))))))),
  if M =:= NewM ->
      M;
     true ->
      refine(NewM)
  end.

refine_rows(M) ->
	[refine_row(R) || R <- M].

%% EDIT: Parallel refine_rows function
par_refine_rows([]) ->
	[];
par_refine_rows([Row|Rest]) ->	
	Parent = self(),
	
	Ref = make_ref(),
	
	spawn_link(fun() -> Parent ! {Ref, catch refine_row(Row)} end),
	
	NewRest = par_refine_rows(Rest),
	
	receive
		{Ref, NewRow} -> [NewRow | NewRest] 
	end.
%% EDIT end

%% EDIT: Parallel refine function (parallel rows, columns and blocks)
par_refine(M) ->
	Parent = self(),
	
	Ref_block = make_ref(),
	Ref_col = make_ref(),
	Ref_row = make_ref(),
	
	%% Create process for calculating the blocks
	spawn_link(fun() -> Parent ! {Ref_block, catch unblocks(refine_rows(blocks(M)))} end),
	
	%% Create process for calculating the columns
	spawn_link(fun() -> Parent ! {Ref_col, catch transpose(refine_rows(transpose(M)))} end),
	
	%% Create process for calculating the rows
	spawn_link(fun() -> Parent ! {Ref_row, catch refine_rows(M)} end),
	
	%% Receive blocks result
	M1 = receive
			 {Ref_block, A} -> A 
		 end,
	
	%% Receive columns result
	M2 = receive 
			 {Ref_col, B} -> B
		 end,
	
	%% Receive rows result
	M3 = receive
			 {Ref_row, C} -> C
		 end,
	
	%% Join the three results
	NewM = join_solutions(M1, M2, M3),
	
	if M =:= NewM ->
		   M;
	   true ->
		   refine(NewM)
	end.

join_solutions(M1, M2, M3) ->
	[ begin
		  [ from_list(intersect(C1, intersect(C2, C3)))
		  || {C1, C2, C3} <- lists:zip3(R1, R2, R3)]
	  end
	|| {R1, R2, R3} <- lists:zip3(M1, M2, M3)].

intersect(Ls1, Ls2) ->
	lists:filter(fun(E) -> lists:member(E, to_list(Ls2)) end, to_list(Ls1)).

to_list(X) ->
	if is_list(X) -> X;
	   true ->
		   [X]
	end.

from_list([X]) -> X;
from_list(X) -> X.
%% EDIT end

%% EDIT: Parallel guesses
par_solve_one([], Ref, _) ->
	receive
		{Ref, Solution} -> Solution
	end;
par_solve_one([M|Ms], Ref, Parent) ->
	spawn(fun() -> Parent ! {Ref, catch solve_refined(M)} end),
	par_solve_one(Ms, Ref, Parent).
par_solve_one(Ms) ->
	Ref = make_ref(),
	Parent = self(),
	par_solve_one(Ms, Ref, Parent).

refine_row(Row) ->
  Entries = entries(Row),
  NewRow =
    [if is_list(X) ->
	 case X -- Entries of
	   [] ->
	     exit(no_solution);
	   [Y] ->
	     Y;
	   NewX ->
	     NewX
	 end;
	true ->
	 X
     end
     || X <- Row],
  NewEntries = entries(NewRow),
  %% check we didn't create a duplicate entry
  case length(lists:usort(NewEntries)) =:= length(NewEntries) of
    true ->
      NewRow;
    false ->
      exit(no_solution)
  end.

entries(Row) ->
  [X || X <- Row, is_filled(X)].

is_exit({'EXIT',_}) ->
  true;
is_exit(_) ->
  false.

%% how hard is the puzzle?

hard(M) ->
  lists:sum([lists:sum([hardness(X) || X <- Row]) || Row <- M]).

hardness(X) when is_list(X) -> length(X);
hardness(_) -> 0.

%% choose a position {I, J, Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
  Nine = ?NINE,
  {_, I, J, X} =
    lists:min([{length(X), I, J, X}
	       || {I, Row} <- lists:zip(Nine, M),
		  {J, X} <- lists:zip(Nine, Row),
		  is_list(X)]),
  {I, J, X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M0) ->
  {I, J, Guesses} = guess(M0),
  Ms = [catch refine(update_element(M0, I, J, G)) || G <- Guesses],
  SortedGuesses = lists:sort([{hard(M), M} || M <- Ms, not is_exit(M)]),
  [G || {_, G} <- SortedGuesses].

%% -------------------------------------------------------------------
%% Matrix operations (with some of their testing code)

transpose([Row]) ->
  [[X] || X <- Row];
transpose([Row|M]) ->
  [[X|Xs] || {X, Xs} <- lists:zip(Row, transpose(M))].

-ifdef(PROPER).
prop_transpose() ->
  ?FORALL({M, N}, {nat(), nat()},
	  ?FORALL(Mat, vector(M+1, vector(N+1, elem())),
		  transpose(transpose(Mat)) =:= Mat)).
-endif.

update_element(M, I, J, G) ->
  update_nth(I, update_nth(J, G, lists:nth(I, M)), M).

update_nth(I, X, Xs) ->
  {Pre, [_|Post]} = lists:split(I-1, Xs),
  Pre ++ [X|Post].

-ifdef(PROPER).
prop_update() ->
  ?FORALL(L, list(integer()),
	  ?IMPLIES(L =/= [],
		   ?FORALL(I, choose(1, length(L)),
			   update_nth(I, lists:nth(I, L), L) =:= L))).
-endif.

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
  [[A,B,C]|triples(D)];
triples([]) ->
  [].

blocks(M) ->
  Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
  lists:append([[lists:append(X) || X <- B] || B <- Blocks]).

unblocks(M) ->
  [lists:append(X)
   || X <- transpose([lists:append(Y)
		      || Y <- [[triples(T) || T <- Ts] || Ts <- triples(M)]])].

-ifdef(PROPER).
prop_blocks() ->
  ?FORALL(M, vector(9, vector(9, elem())), unblocks(blocks(M)) =:= M).
-endif.
