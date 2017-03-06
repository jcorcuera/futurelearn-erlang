-module(exercises_2_6).

-include_lib("eunit/include/eunit.hrl").

-export([product/1, product_tail/1, maximum/1, maximum_tail/1]).

%% Combining list elements: the product of a list
%% Using the template from the last session, define an Erlang function to give
%% the product of a list of numbers. The product of an empty list is usually
%% taken to be 1: why?

product([]) ->
  1;

product([H|T]) ->
  H * product(T).

%% Tail

product_tail(List) ->
  product_tail(List, 1).

product_tail([], Acc) ->
  Acc;

product_tail([H|T], Acc) ->
  product_tail(T, H * Acc).


%% Combining list elements: the maximum of a list
%% Define an Erlang function to give the maximum of a list of numbers.
%%
%% You might find it helpful to use the function max/2 that gives the maximum
%% of two values.

maximum([H]) ->
  H;

maximum([H|T]) ->
  max(H, maximum(T)).

%% Tail

maximum_tail([H|T]) ->
  maximum_tail(T, H).

maximum_tail([], Max) ->
  Max;

maximum_tail([H|T], Max) ->
  maximum_tail(T, max(H, Max)).


%% Tests

product_test() ->
  ?assertEqual(6, product([1,2,3])),
  ?assertEqual(-6, product([-1,2,3])),
  ?assertEqual(0, product([1,2,3,8,0])).

product_tail_test() ->
  ?assertEqual(6, product_tail([1,2,3])),
  ?assertEqual(-6, product_tail([-1,2,3])),
  ?assertEqual(0, product_tail([1,2,3,8,0])).

maximum_test() ->
  ?assertEqual(10, maximum([1,10,5,6,3,8])),
  ?assertEqual(-1, maximum([-10,-20,-1,-100,-5])).

maximum_tail_test() ->
  ?assertEqual(10, maximum_tail([1,10,5,6,3,8])),
  ?assertEqual(-1, maximum_tail([-10,-20,-1,-100,-5])).
