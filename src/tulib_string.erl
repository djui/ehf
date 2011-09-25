%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(tulib_string).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ is_str/1
        , format/1  
        , skip_blanks/1
        , trim_blanks/1
        , split_at_char/2
        , replace/3
        , make_test_strings/1
        , test_function_over_substrings/2
        , ndots/1
        , string2term/1
        , term2string/1
        , tolower/1
        , string2value/1
        , string2value/2
        , rtrim/1
        , ltrim/1
        , empty/1
        , split/2
        , foreach_word_in_string/2
        , is_word_char/1
        , get_word/1
        ]).

-define(is_str(S), (S == [] orelse is_integer(hd(S)))).

%% @doc Lazily checks if a given term might be a string.
is_str(S) -> ?is_str(S).

%% @doc Pretty-print any string
format([]) -> "";
format(S0) when ?is_str(S0) ->
  S = case hd(lists:reverse(S0)) of
        $. -> S0;
        _  -> S0 ++ "."
      end,
  {ok,Scan,_} = erl_scan:string(S),
  {ok,Parse}  = erl_parse:parse_exprs(Scan),
  Syntax      = erl_syntax:form_list(Parse),
  PrettyS     = erl_prettypr:format(Syntax),
  PrettyS;
format(S) when ?is_str(S) -> format(S ++ ".");
format(T) -> io:format("~p~n", [T]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks(X)       -> X.
    
%% @copyright Programming Erlang - The Pragmatic Bookshelf
trim_blanks(X) -> lists:reverse(skip_blanks(lists:reverse(X))).
    
%% @copyright Programming Erlang - The Pragmatic Bookshelf
split_at_char(Str, C) -> split_at_char(Str, C, []).
split_at_char([C|T], C, L) -> {yes, lists:reverse(L), T};
split_at_char([H|T], C, L) -> split_at_char(T, C, [H|L]);
split_at_char([], _, _)    -> no.

%% @doc replace and Key with Key,Val in the association list Old
%% @copyright Programming Erlang - The Pragmatic Bookshelf
replace(Key, Val, Old) -> replace(Key, Val, Old, []).
replace(Key, Val1, [{Key,_Val}|T], L) -> lists:reverse(L, [{Key, Val1}|T]);
replace(Key, Val, [H|T], L)           -> replace(Key, Val, T, [H|L]);
replace(Key, Val, [], L)              -> [{Key,Val}|L].

%% @copyright Programming Erlang - The Pragmatic Bookshelf
make_test_strings(Str) -> L = length(Str), make_test_strings(Str, L+1, 1).
make_test_strings(_, Max, Max) -> [];
make_test_strings(Str, Max, N) ->
  [string:sub_string(Str, 1, N)|make_test_strings(Str, Max, N+1)].

%% @copyright Programming Erlang - The Pragmatic Bookshelf
test_function_over_substrings(F, Str) ->
  L = make_test_strings(Str),
  lists:foreach(fun(S) ->
              io:format("|~s|~n    => ~p~n", [S, F(S)])
          end, L).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
ndots([$.|T]) -> 1 + ndots(T);
ndots([_|T])  -> ndots(T);
ndots([])     -> 0.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
string2term(Str) ->
  {ok, Tokens,_} = erl_scan:string(Str ++ "."),
  {ok, Term} = erl_parse:parse_term(Tokens),
  Term.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
term2string(Term) ->
  lists:flatten(io_lib:format("~p",[Term])).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
tolower(Str) -> lists:map(fun do_tolower/1, Str).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
do_tolower(X) when $A =< X, X =< $Z -> X+ $a - $A;
do_tolower(X)                       -> X.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
string2value(Str) ->
  {ok, Tokens, _} = erl_scan:string(Str ++ "."),
  {ok, Exprs} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
  Value.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
string2value(Str, Bindings0) ->
  {ok, Tokens, _} = erl_scan:string(Str ++ "."),
  {ok, Exprs} = erl_parse:parse_exprs(Tokens),
  {value, Value, Bindings1} = erl_eval:exprs(Exprs, Bindings0),
  {Value, Bindings1}.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
rtrim([$\n|T]) -> rtrim(T);
rtrim([$\s|T]) -> rtrim(T);
rtrim([$\t|T]) -> rtrim(T);
rtrim(X) -> X.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
ltrim(X) -> lists:reverse(rtrim(lists:reverse(X))).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
empty([$\s|T]) -> empty(T);
empty([$\n|T]) -> empty(T);
empty([$\r|T]) -> empty(T);
empty([$\t|T]) -> empty(T);
empty([]) -> true;
empty(_)  -> false.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
split(F, L) -> split(F, L, [], []).
split(F, [H|T], True, False) ->
  case F(H) of
    true  -> split(F, T, [H|True], False);
    false -> split(F, T, True, [H|False])
  end;
split(_, [], True, False) -> {lists:reverse(True), lists:reverse(False)}.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
foreach_word_in_string(Str, F) ->
  case get_word(Str) of
    no -> 
      void;
    {Word, Str1} ->
      F(Word),
      foreach_word_in_string(Str1, F)
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
is_word_char(X) when $A=< X, X=<$Z -> true;
is_word_char(X) when $0=< X, X=<$9 -> true;
is_word_char(X) when $a=< X, X=<$z -> true;
is_word_char(_)  -> false.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
get_word([H|T]) ->
    case is_word_char(H) of
      true  -> collect_word(T, [H]);
      false -> get_word(T)
    end;
get_word([]) -> no.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
collect_word([H|T]=All, L) ->
  case is_word_char(H) of
    true  -> collect_word(T, [H|L]);
    false -> {lists:reverse(L), All}
  end;
collect_word([], L) ->
  {lists:reverse(L), []}.
