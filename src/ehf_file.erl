%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(ehf_file).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ download/1
        , download/2
        , download_to_file/2
        , download_to_file/3
        , tempfile/0
        , tempfile/1
        , file_size_and_type/1
        , ls/1
        , consult/1
        , unconsult/2
        , dump/2
        , read_file_as_lines/1
        , out_of_date/2
        , last_modified/1
        , exists/1
        , term2file/2
        , file2term/1
        , eval_file/1
        , extract_attribute/2
        , foreach_word_in_file/2
        ]).

%% @doc Download a file via HTTP GET request with a default timeout of 30s.
download(Url) -> download(Url, 30000).
%% @doc Download a file via HTTP GET request with a given timeout.
download(Url, Timeout) ->
  inets:start(),
  ssl:start(),
  
  {ok, {{"HTTP/1.1", 200, "OK"}, _Header, Body}} =
    httpc:request(get, {Url, []}, [{timeout, Timeout}], []),
  Body.                                                                     

%% @doc Download a file via HTTP GET request with a default timeout of 30s and 
%% store it in a given filename.
download_to_file(Url, Filename) -> download_to_file(Url, Filename, 30000).
%% @doc Download a file via HTTP GET request with a given timeout and store it 
%% in a given filename.
download_to_file(Url, Filename, Timeout) ->
  Content = download(Url, Timeout),
  file:write_file(Filename, Content).

%% @doc Create unique filename in temp directory. Note: The fiel creation is not
%% atomically done, thereby prone to race conditions; only a probabilistic
%% approach.
%% DIR/[PREFIX-]RANDOM-TIMESTAMP.tmp
tempfile() -> tempfile("").
tempfile(Prefix) ->
  Random = integer_to_list(random:uniform(1000000)),
  prepare_tempfile("/tmp/", Prefix, Random, ".tmp").

prepare_tempfile(Dir, "", Random, Extension) ->
  do_tempfile(Dir ++ Random ++ "-", Extension);
prepare_tempfile(Dir, Prefix, Random, Extension) ->
  do_tempfile(Dir ++ Prefix ++ "-" ++ Random ++ "-", Extension).

do_tempfile(Filename, Extension) ->
  Filename2 = test_server:temp_name(Filename) ++ Extension,
  ok = file:write_file(Filename2, ""),
  Filename2.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
-include_lib("kernel/include/file.hrl").
file_size_and_type(File) ->
  case file:read_file_info(File) of
    {ok, Facts} -> {Facts#file_info.type, Facts#file_info.size};
    _           -> error
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
ls(Dir) ->
  {ok, L} = file:list_dir(Dir),
  lists:map(fun(I) -> {I, file_size_and_type(I)} end, lists:sort(L)).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
consult(File) ->
  case file:open(File, read) of
    {ok, S} ->
      Val = do_consult(S),
      file:close(S),
      {ok, Val};
    {error, Why} ->
      {error, Why}
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
do_consult(S) ->
  case io:read(S, '') of
    {ok, Term} -> [Term|do_consult(S)];
    eof        -> [];
    Error      -> Error
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
unconsult(File, L) ->
  {ok, S} = file:open(File, write),
  lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L),
  file:close(S).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
dump(File, Term) ->
  Out = File ++ ".tmp",
  io:format("** dumping to ~s~n",[Out]),
  {ok, S} = file:open(Out, [write]),
  io:format(S, "~p.~n",[Term]), 
  file:close(S).

%% @doc read file into line buffer
%% @copyright Programming Erlang - The Pragmatic Bookshelf
read_file_as_lines(File) ->    
  case file:read_file(File) of
    {ok, Bin} ->
      {ok, split_into_lines(binary_to_list(Bin), 1,  [])};
    {error, _} ->
      {error, eNoFile}
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
split_into_lines([], _, L) -> lists:reverse(L);
split_into_lines(Str, Ln, L) ->
  {Line, Rest} = get_line(Str, []),
  split_into_lines(Rest, Ln+1, [{Ln,Line}|L]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
get_line([$\n|T], L) -> {lists:reverse(L), T};
get_line([H|T], L)   -> get_line(T, [H|L]);
get_line([], L)      -> {lists:reverse(L), []}.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
out_of_date(In, Out) ->
  case exists(Out) of
    true ->
      case {last_modified(In), last_modified(Out)} of
        {T1, T2} when T1 > T2 ->
          true;
        _ ->
          false
      end;
    false ->
      true
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
last_modified(File) ->
  case file:read_file_info(File) of
    {ok, Info} ->
      Info#file_info.mtime;
    _ ->
      0
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
exists(File) ->
  case file:read_file_info(File) of
    {ok, _} ->
      true;
    _ ->
      false
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
term2file(File, Term) -> file:write_file(File, term_to_binary(Term)).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
file2term(File) ->
  {ok, Bin} = file:read_file(File),
  binary_to_term(Bin).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
eval_file(File) ->
  {ok, S} = file:open(File, [read]),
  Vals = eval_file(S, 1, erl_eval:new_bindings()),
  file:close(S),
  Vals.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
eval_file(S, Line, B0) ->
  case io:parse_erl_exprs(S, '', Line) of
    {ok, Form, Line1} ->
      {value, Value, B1} = erl_eval:exprs(Form, B0),
      [Value|eval_file(S, Line1, B1)];
    {eof, _} ->
      []
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
extract_attribute(File, Key) ->
  case beam_lib:chunks(File,[attributes]) of
    {ok, {attrs, [{attributes,L}]}} ->
      lists:lookup(Key, L);
    _ -> exit(badFile)
  end.

%% @doc Evalute F(Word) for each word in the file File
%% @copyright Programming Erlang - The Pragmatic Bookshelf
foreach_word_in_file(File, F) ->
  case file:read_file(File) of
    {ok, Bin} -> string:foreach_word_in_string(binary_to_list(Bin), F);
    _         -> void
  end.
