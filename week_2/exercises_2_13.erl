-module(exercises_2_13).

-include_lib("eunit/include/eunit.hrl").

-export([nub/1]).

%% Define a function nub to remove all the duplicate elements from a list.

-spec nub([T]) -> [T].

nub(List) ->
  nub(List, []).

nub([], Result) ->
  lists:reverse(Result);

nub([H|T], Result) ->
  case lists:member(H, Result) of
    false -> nub(T, [H|Result]);
    _ -> nub(T, Result)
  end.

%% Specs

nub_test() ->
  ?assertEqual([1,2,3,4], nub([1,1,1,2,3,1,2,4,3,3])),
  ?assertEqual([], nub([])).
