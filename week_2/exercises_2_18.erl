-module(exercises_2_18).

-include_lib("eunit/include/eunit.hrl").

-export([join/2, concat/1, member/2]).
-export([merge_sort/1, quick_sort/1, insertion_sort/1]).
-export([perms/1]).

%% Joining lists together
%%
%% Here we consider two of the principal functions over lists. The ++ operator
%% joins two lists together, and lists:concat/1 joins a list of lists into a
%% single list. For example:
%%
%% "hel"++"lo" = "hello"
%% lists:concat(["goo","d","","by","e"]) = "goodbye"
%% Write your own definitions of these functions. In the case of ++ you’ll need
%% to define a function - say, join/2, as you can’t define your own operators
%% in Erlang.
%%
%% Hint: Think about how you could use join (or ++) in the definition of concat.

join(L1, L2) ->
  join(L1, L2, []).


join([], [], Result) ->
  lists:reverse(Result);

join([], [H|T], Result) ->
  join([], T, [H|Result]);

join([H|T], L2, Result) ->
  join(T, L2, [H|Result]).


join_test() ->
  ?assertEqual("hello", join("hel", "lo")).



concat([L1, L2 | T]) ->
  concat([join(L1, L2)| T]);

concat([L]) ->
  L.

concat_test() ->
  ?assertEqual("goodbye", concat(["goo","d","","by","e"])).


%%
%% Testing membership
%%
%% Define a function member/2 that tests whether its first argument is a member of its second argument, which is a list. For example:
%%
%% member(2,[2,0,0,1]) = true
%% member(20,[2,0,0,1]) = false

member(_, []) ->
  false;

member(E, [E|_]) ->
  true;

member(E, [_|T]) ->
  member(E, T).

member_test() ->
  ?assertEqual(true, member(2,[2,0,0,1])),
  ?assertEqual(false, member(20,[2,0,0,1])).



%% Sorting lists
%%
%% A list can be sorted in a number of ways, including these algorithms
%% described informally:
%%

%%
%% Merge sort: divide the list into two halves of (approximately) equal length,
%% sort them (recursively) and then merge the results.

merge_sort([H]) ->
  [H];

merge_sort(List) ->
  {L1, L2} = split_list(List),
  SL1 = merge_sort(L1),
  SL2 = merge_sort(L2),
  merge(SL1, SL2).

split_list(List) ->
  Index = length(List) div 2,
  lists:split(Index, List).

merge(SL1, SL2) ->
  merge_tail(SL1, SL2, []).

merge_tail([], [], Sorted) ->
  lists:reverse(Sorted);

merge_tail([H1|T1], [], Sorted) ->
  merge_tail(T1, [], [H1|Sorted]);

merge_tail([], [H2|T2], Sorted) ->
  merge_tail([], T2, [H2|Sorted]);

merge_tail([H1|T1], SL2=[H2|_], Sorted) when H1 < H2 ->
  merge_tail(T1, SL2, [H1|Sorted]);

merge_tail(SL1= [H1|_], [H2|T2], Sorted) when H2 =< H1 ->
  merge_tail(SL1, T2, [H2|Sorted]).

%% Tests

merge_sort_test() ->
  ?assertEqual([1,2,3,4,5,6,7], merge_sort([7,3,4,5,2,1,6])),
  ?assertEqual([1,2,3,4,5,6], merge_sort([6,3,4,5,1,2])).


%%
%% Quicksort: split the list into two according to whether the items are smaller
%% than (or equal to) or larger than the pivot, often taken to be the head
%% element of the list; sort the two halves and join the results together.
%%

quick_sort([]) ->
  [];

quick_sort([H]) ->
  [H];

quick_sort([Pivot|T]) ->
  {Lower, Greater} = split_by_pivot(Pivot, T),
  SL1 = quick_sort(Lower),
  SL2 = quick_sort(Greater),
  SL1 ++ [Pivot|SL2].


split_by_pivot(Pivot, List) ->
  split_by_pivot_tail(Pivot, List, [], []).

split_by_pivot_tail(_, [], Lower, Greater) ->
  {Lower, Greater};

split_by_pivot_tail(Pivot, [H|T], Lower, Greater) when H < Pivot ->
  split_by_pivot_tail(Pivot, T, [H|Lower], Greater);

split_by_pivot_tail(Pivot, [H|T], Lower, Greater) when H >= Pivot ->
  split_by_pivot_tail(Pivot, T, Lower, [H|Greater]).

quick_sort_test() ->
  ?assertEqual([1,2,3,4,5,6,7], quick_sort([7,3,4,5,2,1,6])),
  ?assertEqual([1,2,3,4,5,6], quick_sort([6,3,4,5,1,2])).



%%
%% Insertion sort: sort the tail of the list and then insert the head of the list in the correct place.
%%

insertion_sort(List) ->
  insertion_sort(List, []).

insertion_sort([], Sorted) ->
  Sorted;

insertion_sort([H|T], Sorted) ->
  NewSorted = insert(H, Sorted, []),
  insertion_sort(T, NewSorted).

insert(E, [], Result) ->
  lists:reverse([E|Result]);

insert(E, List=[H|_T], Result) when E =< H->
  lists:reverse(Result) ++ [E|List];

insert(E, [H|T], Result) ->
  insert(E, T, [H|Result]).

insertion_sort_test() ->
  ?assertEqual([1,2,3,4,5,6,7], insertion_sort([7,3,4,5,2,1,6])),
  ?assertEqual([1,2,3,4,5,6], insertion_sort([6,3,4,5,1,2])).

%%
%% Permutations
%% A permutation of a list xs consists of the same elements in a (potentially) different order. Define a function that gives all the permutations of a list, in some order. For example:
%%
%% perms([]) = [[]]
%% perms([1,2,3]) = [[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]]

perms([]) ->
  [[]];

perms(List) ->
  perms(List, [], []).

perms([], [], _Result) ->
  [[]];

perms([], _PermutedElements, Result) ->
  lists:reverse(Result);

perms([H|T], PermutedElements, Result) ->
  Permutations = perms(PermutedElements ++ T, [], []),
  NewResults = add_to_permutations(H, Permutations, []),
  perms(T, [H|PermutedElements], NewResults ++ Result).

add_to_permutations(_E, [], Acc) ->
  Acc;

add_to_permutations(E, [H|T], Acc) ->
  add_to_permutations(E, T, [[E|H]|Acc]).

perms_test() ->
  ?assertEqual([[1]], perms([1])),
  ?assertEqual([[1,2], [2,1]], perms([1,2])),
  ?assertEqual([[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,2,1],[3,1,2]], perms([1,2,3])).
