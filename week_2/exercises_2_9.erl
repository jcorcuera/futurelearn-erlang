-module(exercises_2_9).
-export([double/1, evens/1]).

%% The aim of these exercises is to familiarise you with other ways of defining
%% functions over lists in Erlang, in particular the different way that
%% recursive functions can construct lists.


%% Transforming list elements
%% Define an Erlang function double/1 to double the elements of a list of numbers.

double(List) ->
  lists:reverse(double(List, [])).

double([], Result) ->
  Result;

double([H|T], Result) ->
  double(T, [H * 2 | Result]).

%% Filtering lists
%% Define a function evens/1 that extracts the even numbers from a list of integers.

evens(List) ->
  lists:reverse(evens(List, [])).

evens([], Result) ->
  Result;

evens([H|T], Result) ->
  case H rem 2 of
    0 -> evens(T, [H|Result]);
    _ -> evens(T, Result)
  end.
