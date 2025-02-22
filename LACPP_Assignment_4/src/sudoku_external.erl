-module(sudoku_external).
%-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose

transpose([Row]) ->
  [[X] || X <- Row];
transpose([Row|M]) ->
  [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%%      ?FORALL(Mat,matrix(M+1,N+1),
%%        transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
  [[A,B,C]|triples(D)];
triples([]) ->
  [].

blocks(M) ->
  Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
  lists:append(
    lists:map(fun(X)->
                  lists:map(fun lists:append/1, X)
              end,
              Blocks)).

unblocks(M) ->
  lists:map(
    fun lists:append/1,
    transpose(
      lists:map(
        fun lists:append/1,
        lists:map(
          fun(X)->lists:map(fun triples/1,X) end,
          triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%%      unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
  [X || X <- Row,
        1 =< X andalso X =< 9].

safe_entries(Row) ->
  Entries = entries(Row),
  lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
  lists:all(fun safe_entries/1,M).

safe(M) ->
  safe_rows(M) andalso
  safe_rows(transpose(M)) andalso
  safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
  Nine = lists:seq(1,9),
  [[if 1=<X, X=<9 ->
         X;
       true ->
         Nine
    end
    || X <- Row]
   || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
  NewM =
  refine_rows(
    transpose(
      refine_rows(
        transpose(
          unblocks(
            refine_rows(
              blocks(M))))))),
  if M==NewM ->
       M;
     true ->
       refine(NewM)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parallelzation of Row, Columns & Blocks %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Use this function isntead of refine/1 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p_block_refine(M) ->
  flush(),
  Parent = self(),
  BlockRef = make_ref(),
  ColRef = make_ref(),
  RowRef = make_ref(),
  spawn_link(fun() ->
                 Parent ! {BlockRef, catch unblocks(
                                       refine_rows(
                                         blocks(M)))}
             end),
  spawn_link(fun() ->
                 Parent ! {ColRef, catch transpose(
                                       refine_rows(
                                         transpose(M)))}
             end),
  spawn_link(fun() ->
                 Parent ! {RowRef, catch refine_rows(M)}
             end),
  M1 = receive {BlockRef, {'EXIT',_}} -> exit(no_solution);
               {BlockRef, X} -> X end,
  M2 = receive {ColRef, {'EXIT',_}} -> exit(no_solution);
               {ColRef, Y} -> Y end,
  M3 = receive {RowRef, {'EXIT',_}} -> exit(no_solution);
               {RowRef, Z} -> Z end,
  NewM = join_solutions(M1,M2,M3),
  if M==NewM ->
       M;
     true ->
       refine(NewM)
  end.

join_solutions(M1,M2,M3) ->
  [ begin
      [ from_list(intersect(C1,intersect(C2,C3)))
       || {C1,C2,C3} <- lists:zip3(R1,R2,R3)]
    end
   || {R1,R2,R3} <- lists:zip3(M1,M2,M3)].

intersect(L1,L2) ->
  lists:filter(fun(E) -> lists:member(E,to_list(L2)) end,to_list(L1)).

to_list(X) ->
  if is_list(X) -> X;
     true       -> [X]
  end.

from_list([X]) -> X;
from_list(X) -> X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Parallelization of row %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Use this instead of refine/1 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% refine with parallel rows (really bad idea)
p_row_refine(M) ->
  NewM =
  p_refine_rows(
    transpose(
      refine_rows(
        transpose(
          unblocks(
            refine_rows(
              blocks(M))))))),
  if M==NewM ->
       M;
     true ->
       p_row_refine(NewM)
  end.

p_refine_rows([]) ->
  [];
p_refine_rows([Row|Rest]) ->
  Parent = self(),
  Ref = make_ref(),
  spawn_link(fun() ->
            Parent ! {Ref, catch refine_row(Row)}
        end),
  NewRest = p_refine_rows(Rest),
  receive
    % vi behöver använda flush() för att inte ackumulera meddelanden.
    % exit() behövs för att vi inte ska fortsätta leta när något har gått snett
    % men då är det en massa processer som inte får sina meddelanden lästa.
    {Ref, {'EXIT', _}} -> exit(no_solution);
    {Ref, NewRow}      -> [NewRow | NewRest]
  end.

refine_rows(M) ->
  lists:map(fun refine_row/1,M).

refine_row(Row) ->
  Entries = entries(Row),
  NewRow =
  [if is_list(X) ->
        case X--Entries of
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
  case length(lists:usort(NewEntries)) == length(NewEntries) of
    true ->
      NewRow;
    false ->
      exit(no_solution)
  end.

% flush the message queue

flush() ->
  receive _ -> flush()
  after 0   -> ok
  end.

is_exit({'EXIT',_}) ->
  true;
is_exit(_) ->
  false.

%% is a puzzle solved?

solved(M) ->
  lists:all(fun solved_row/1,M).

solved_row(Row) ->
  lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->
  lists:sum(
    [lists:sum(
       [if is_list(X) ->
             length(X);
           true ->
             0
        end
        || X <- Row])
     || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
  Nine = lists:seq(1,9),
  {_,I,J,X} =
  lists:min([{length(X),I,J,X}
             || {I,Row} <- lists:zip(Nine,M),
                {J,X} <- lists:zip(Nine,Row),
                is_list(X)]),
  {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M) ->
  {I,J,Guesses} = guess(M),
  Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
  SortedGuesses =
  lists:sort(
    [{hard(NewM),NewM}
     || NewM <- Ms,
        not is_exit(NewM)]),
  [{H,G} || {H,G} <- SortedGuesses].

update_element(M,I,J,G) ->
  update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
  {Pre,[_|Post]} = lists:split(I-1,Xs),
  Pre++[X|Post].

%% solve a puzzle

solve(M) ->
  Solution = solve_refined(refine(fill(M))),
  case valid_solution(Solution) of
    true ->
      Solution;
    false ->
      exit({invalid_solution,Solution})
  end.

solve_refined(M) ->
  case solved(M) of
    true ->
      M;
    false ->
      solve_one(guesses(M))
  end.

solve_one_seq([]) ->
  exit(no_solution);
solve_one_seq([{_,M}]) ->
  % io:format("~p~n",[H]),
  solve_refined(M);
solve_one_seq([{_,M}|Ms]) ->
  case catch solve_refined(M) of
    {'EXIT',no_solution} ->
      % io:format("~p~n",[H]),
      solve_one_seq(Ms);
    Solution ->
      Solution
  end.

%% solve_one([]) ->
%%   exit(no_solution);
%% solve_one([{_,M}]) ->
%%   solve_refined(M);
%% solve_one([{_,M}|Ms]) ->
%%   case catch solve_refined(M) of
%%    {'EXIT',no_solution} ->
%%       solve_one_seq(Ms);
%%     Solution ->
%%       Solution
%%   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Parallelizing Guess %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Replace the solve_one %%%
% above with the one below  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_one([],Ref,_) ->
	wait_for_solution(Ref);
solve_one([{_,M}|Ms],Ref,Parent) ->
   spawn(fun() ->
             Parent ! {Ref, catch solve_refined(M)}
         end),
        solve_one(Ms,Ref,Parent).
 solve_one(Ms) ->
   Ref = make_ref(),
   Parent = self(),
   solve_one(Ms,Ref,Parent).

wait_for_solution(Ref) ->
  receive
    {Ref, {'EXIT',_}} -> wait_for_solution(Ref);
    {Ref, Solution}   -> flush(), Solution
  end.

%% benchmarks

-define(EXECUTIONS,42).
% this is the problem that benchmark/0 runs
-define(PROBLEM,real_challenge).

bm(F) ->
  {T,_} = timer:tc(?MODULE,repeat,[F]),
  T/?EXECUTIONS/1000.

repeat(F) ->
  [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
  [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
  {ok,Puzzles} = file:consult("sudoku_problems.txt"),
  timer:tc(?MODULE,benchmarks,[Puzzles]).

% used by benchmark/0, selects one problem and solves it
benchmark(Puzzles) ->
  [{?PROBLEM,bm(fun() -> solve(M) end)} || {?PROBLEM,M} <- Puzzles].

% benchmark one particular problem
benchmark() ->
  {ok,Puzzles} = file:consult("sudoku_problems.txt"),
  timer:tc(?MODULE,benchmark,[Puzzles]).

p_benchmarks([{Name,M}]) -> [{Name,bm(fun() -> solve(M) end)}];
p_benchmarks([{Name,M}|Puzzles]) ->
  Parent = self(),
  Ref = make_ref(),
  spawn_link(fun() -> Parent ! {Ref, bm(fun() -> solve(M) end)} end),
  Solutions = p_benchmarks(Puzzles),
  receive {Ref,Solution} -> [{Name,Solution}|Solutions] end.

p_benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,p_benchmarks,[Puzzles]).

%% check solutions for validity

valid_rows(M) ->
  lists:all(fun valid_row/1,M).

valid_row(Row) ->
  lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
  valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).

