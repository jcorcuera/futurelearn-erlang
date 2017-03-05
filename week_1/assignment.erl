-module(assignment).
-export([perimeter/1, area/1, enclose/1, bits/1, bits_tail/1]).

%% Shapes
%%
%% {circle, {X,Y}, R}
%% {rectangle, {X,Y}, H, W}
%% {triangle, {B,H}, S1, S2, S3 }
%%
%% Define a function perimeter/1 which takes a shape and returns the perimeter
%% of the shape.
%% Choose a suitable representation of triangles, and augment area/1 and
%% perimeter/1 to handle this case too.
%% Define a function enclose/1 that takes a shape an returns the smallest
%% enclosing rectangle of the shape.

perimeter({circle, {_, _}, R}) ->
  2 * math:pi() * R;

perimeter({rectangle, {_, _}, H, W}) ->
  2 * (H + W);

perimeter({triangle, {_, _}, S1, S2, S3}) ->
  S1 + S2 + S3.


area({circle, {_, _}, R}) ->
  math:pi() * R * R;

area({rectangle, {_, _}, H, W}) ->
  H * W;

area({triangle, {B, H}, _, _, _}) ->
  B * H / 2.


enclose({circle, {X, Y}, R}) ->
  {rectangle, {X, Y}, R*2, R*2};

enclose({rectangle, {X, Y}, H, W}) ->
  {rectangle, {X, Y}, H, W};

enclose({triangle, {B, H}, _, _, _}) ->
  {rectangle, {B/2, H/2}, B, H}.


%% Summing the bits
%%
%% Define a function bits/1 that takes a positive integer N and returns the sum
%% of the bits in the binary representation. For example bits(7) is 3 and
%% bits(8) is 1.

%% Direct Recursive

bits(0) ->
  0;

bits(N) when N >= 0 ->
  Remainder = N rem 2,
  Remainder + bits(N div 2).


%% Tail Recursive

bits_tail(N) when N >= 0 ->
  bits_tail(N, 0).

bits_tail(0, Sum) ->
  Sum;

bits_tail(N, Sum) ->
  Remainder = N rem 2,
  bits_tail(N div 2, Remainder + Sum).


