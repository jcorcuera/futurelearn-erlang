%% Tail Recursion

-module(exercises_1_21).
-include_lib("eunit/include/eunit.hrl").
-export([fib/1, perfect/1]).

fib(N) ->
  fib(N, 0, 1).

fib(0, Previous, _Current) ->
  Previous;
fib(N, Previous, Current) when N > 0 ->
  fib(N-1, Current, Previous + Current).

%% Tests

fib_test() ->
  3 = fib(4),
  8 = fib(6),
  21 = fib(8).

%% Perfect Numbers
%% A positive integer is perfect when it is the sum of its divisors,
%% e.g. 6=1+2+3, 28=1+2+4+7+14.
%% Define a function perfect/1 that takes a positive number N and returns a
%% boolean which indicates whether or not the number is perfect.

perfect(N) ->
  perfect(N, N-1, 0).

perfect(N, 0, Acc) ->
  N == Acc;
perfect(N, Divisor, Acc) when N rem Divisor == 0 ->
  perfect(N, Divisor - 1, Acc + Divisor);
perfect(N, Divisor, Acc) ->
  perfect(N, Divisor - 1, Acc).

%% Tests

perfect_test() ->
  false = perfect(4),
  true = perfect(6),
  false = perfect(12),
  true = perfect(28),
  false = perfect(342),
  true = perfect(496).
