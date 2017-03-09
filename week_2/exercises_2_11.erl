-module(exercises_2_11).
-export([take/2]).

%% Define a function take that takes the first N elements from a list. Here are some examples of take in action:
%%
%% take(0,"hello") = []
%% take(4,"hello") = "hell"
%% take(5,"hello") = "hello"
%% take(9,"hello") = "hello"

-spec take(integer(), [T]) -> [T].

take(N, List) ->
  take(N, List, []).

take(0, _List, Result) ->
  lists:reverse(Result);

take(_N, [], Result) ->
  lists:reverse(Result);

take(N, [H|T], Result) when N > 0 ->
  take(N-1, T, [H| Result]).
