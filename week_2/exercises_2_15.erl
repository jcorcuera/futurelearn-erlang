-module(exercises_2_15).

-include_lib("eunit/include/eunit.hrl").

-export([palindrome/1]).

-spec palindrome(string()) -> boolean().

palindrome(S) ->
  palindrome(S, []).

palindrome([], Result) ->
  Result == lists:reverse(Result);

palindrome([H|T], Result)  when (H >= "a" andalso H =< "z") orelse (H >= "A" andalso H =< "Z") ->
  palindrome(T, [H|Result]);

palindrome([_|T], Result) ->
  palindrome(T, Result).

palindrome_test() ->
  ?assertEqual(palindrome("Madam I\'m Adam"), true).
