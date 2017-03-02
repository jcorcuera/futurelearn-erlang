%% Pattern Matching

-module(exercises_1_15).
-include_lib("eunit/include/eunit.hrl").
-export([xor_1/2, xor_2/2, xor_3/2, xor_4/2, xor_5/2, maxThree/3, howManyEqual/3]).


%% Exclusive or (xor)
%% In the previous video step on pattern matching we saw two ways of defining
%% “exclusive or”. Give at least three others.

xor_1(true, false) ->
  true;
xor_1(false, true) ->
  true;
xor_1(_,_) ->
  false.

xor_2(X,X) ->
  false;
xor_2(_,_) ->
  true.

xor_3(X,Y) ->
  X =/= Y.

xor_4(true,X) ->
  not X;
xor_4(false, X) ->
  X.

xor_5(X,Y) ->
  (X or Y) and (not X or not Y).

%% Tests

xor_3_test() ->
  false = xor_3(true, true),
  false = xor_3(false, false),
  true = xor_3(true, false),
  true = xor_3(false, true).

xor_4_test() ->
  false = xor_4(true, true),
  false = xor_4(false, false),
  true = xor_4(true, false),
  true = xor_4(false, true).

xor_5_test() ->
  false = xor_5(true, true),
  false = xor_5(false, false),
  true = xor_5(true, false),
  true = xor_5(false, true).


%% Maximum of three
%% Give a definition of the function maxThree which takes three integers and
%% returns the maximum of the three. You can use the max function, which gives
%% the maximum of two numbers, in writing your definition.
%%
%% maxThree(34,25,36) = 36

maxThree(X,Y,Z) ->
  max(max(X,Y),Z).

%% Tests

maxThree_test() ->
  3 = maxThree(1,2,3),
  1 = maxThree(1,0,-1),
  2 = maxThree(1,2,1).


%% How many equal?
%% Give a definition of the function howManyEqual which takes three integers and
%% returns an integer, counting how many of its three arguments are equal, so
%% that:
%%
%% howManyEqual(34,25,36) = 0
%% howManyEqual(34,25,34) = 2
%% howManyEqual(34,34,34) = 3

howManyEqual(X,X,X) ->
  3;
howManyEqual(X,X,_) ->
  2;
howManyEqual(X,_,X) ->
  2;
howManyEqual(_,X,X) ->
  2;
howManyEqual(_,_,_) ->
  0.

%% Tests

howManyEqual_test() ->
  3 = howManyEqual(1,1,1),
  2 = howManyEqual(1,1,2),
  2 = howManyEqual(1,2,1),
  2 = howManyEqual(2,1,1),
  0 = howManyEqual(1,2,3).
