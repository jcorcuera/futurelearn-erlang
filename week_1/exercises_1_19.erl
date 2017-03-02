%% "Direct" recursion

-module(exercises_1_19).
-include_lib("eunit/include/eunit.hrl").
-export([fib/1, pieces/1]).

%% Fibonacci numbers - No Tail Recursion
%% The Fibonacci sequence is given by 0, 1, 1, 2, 3, 5, … where subsequent values
%% are given by adding the two previous values in the sequence.

fib(N) when N =< 1, N >= 0 ->
  N;
fib(N) when N > 1 ->
  fib(N-1) + fib(N-2).

%% Tests

fib_test() ->
  3 = fib(4),
  8 = fib(6),
  21 = fib(8).

%% How many pieces?
%% Define a function pieces so that pieces(N) tells you the maximum number of
%% pieces into which you can cut a piece of paper with N straight line cuts.

pieces(0) ->
  1;
pieces(N) when N > 0 ->
  pieces(N-1) + N.

%% Tests

pieces_test() ->
  1 = pieces(0),
  7 = pieces(3),
  11 = pieces(4).
