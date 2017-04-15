-module(index).
-export([get_file_contents/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  Lines = lists:reverse(Rev),
  UnorderedResult = parse_lines(Lines),
  Result = lists:sort(UnorderedResult),
  file:write_file(string:join(["output-", Name], ""), io_lib:fwrite("~p.\n", [Result])).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof ->
      file:close(File),
      Partial;
    Line ->
      {Strip,_} = lists:split(length(Line)-1,Line),
      get_all_lines(File,[Strip|Partial])
  end.

%% Parse Lines
%% Check if line is empty, otherwise extract words longer than 3 characters.
%% Pass lists of words into a dictionary building function.

parse_lines(Lines) ->
  parse_lines(Lines, 1, []).

parse_lines([], _LineNumber, Dictionary) ->
  Dictionary;

parse_lines([L|Ls], LineNumber, Dictionary) when length(L) =:= 0 ->
  parse_lines(Ls, LineNumber + 1, Dictionary);

parse_lines([L|Ls], LineNumber, Dictionary) ->
  case re:run(L, "[A-Za-z]{4,}", [global, {capture, all, list}]) of
    {match, Words} ->
      NewDictionary = update_dictionary(Words, LineNumber, Dictionary),
      parse_lines(Ls, LineNumber + 1, NewDictionary);
    _ ->
      parse_lines(Ls, LineNumber + 1, Dictionary)
  end.


%% Update Dictionary
%% Takes each word, normalizes them, searchs the word in the dictionary,
%% extract the word from the dictionary to later insert an updated one.

update_dictionary([], _LineNumber, Dictionary) ->
  Dictionary;

update_dictionary([[W]|Ws], LineNumber, Dictionary) ->
  NormalizedWord = string:to_lower(W),
  case lists:keytake(NormalizedWord, 1, Dictionary) of
    {value, {NormalizedWord, WordIndexes}, NewDictionary} ->
      NewIndexes = add_index(LineNumber, WordIndexes),
      update_dictionary(Ws, LineNumber, [{NormalizedWord, NewIndexes} | NewDictionary]);
    _ ->
      update_dictionary(Ws, LineNumber, [{NormalizedWord, [{LineNumber, LineNumber}]} | Dictionary])
  end.


%% Add Index
%% Check if the index needs to be updated or just insert a new one.

add_index(LineNumber, Indexes)->
  {First, Last} = lists:last(Indexes),
  case LineNumber =:= Last orelse LineNumber =:= Last + 1 of
    true ->
      NewIndexes = lists:droplast(Indexes),
      NewIndexes ++ [{First, LineNumber}];
    _ ->
      Indexes ++ [{LineNumber, LineNumber}]
  end.
