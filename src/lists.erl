%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(lists).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ empty/1
        , partition/2
        , remove_duplicates/1
        , is_prefix/2
        , first/1
        , duplicates/1
        , complete/2
        , remove_prefix/2
        , longest_common_prefix/1
        , qsort/1
        , perms/1
        ]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
empty([])             -> true;
empty(X) when list(X) -> false.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
partition(F, L) -> partition(F, L, [], []).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
partition(F, [H|T], Yes, No) ->
  case F(H) of
    true  -> partition(F, T, [H|Yes], No);
    false -> partition(F, T, Yes, [H|No])
  end;
%% @copyright Programming Erlang - The Pragmatic Bookshelf
partition(_, [], Yes, No) -> {Yes, No}.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
remove_duplicates(L) -> remove_duplicates(lists:sort(L), []).
%% @copyright Programming Erlang - The Pragmatic Bookshelf
remove_duplicates([H|X=[H|_]], L) -> remove_duplicates(X, L);
remove_duplicates([H|T], L)       -> remove_duplicates(T, [H|L]);
remove_duplicates([], L)          -> L.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
is_prefix([], _)         -> true;
is_prefix([H|T], [H|T1]) -> is_prefix(T, T1);
is_prefix(_, _)          -> false.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
first([_])   -> [];
first([H|T]) -> [H|first(T)].

%% @copyright Programming Erlang - The Pragmatic Bookshelf
duplicates(X) -> find_duplicates(sort(X), []).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
find_duplicates([H,H|T], [H|_]=L) -> find_duplicates(T, L);
find_duplicates([H,H|T], L)       -> find_duplicates(T, [H|L]);
find_duplicates([_|T], L)         -> find_duplicates(T, L);
find_duplicates([], L)            -> L.

%% @doc complete(A, L) -> {yes, S}
%% error     - means no string will ever match
%% {more,L}  - means there are completions but I need more characters
%%             L = [Str] = list of possible completions
%% {yes, S}  - means there is a unique completion
%% 
%% A = S = str(), L=[str()]
%% used to compute the smallest S such that A ++ S is a member of all elements 
%% of L.
%% @copyright Programming Erlang - The Pragmatic Bookshelf
complete(Str, L) ->
  case filter(fun(I) -> is_prefix(Str, I) end, L) of
    []   -> error;
    [L1] -> J = remove_prefix(Str, L1), {yes, J};
    L1   ->
      %% L1 is not empty so it's either more or a string
      %% We know that Str is a prefix of all elements in L1
      L2 = map(fun(I) -> remove_prefix(Str, I) end, L1),
      %% L2 will also not be empty
      %% io:format("L1=~p L2=~p~n",[L1,L2]),
      case longest_common_prefix(L2) of
        [] -> {more, L1};
        S  -> {yes, S}
      end
  end.

%% @doc Find Z such that X ++ Z = Y
%% @copyright Programming Erlang - The Pragmatic Bookshelf
remove_prefix([H|T], [H|T1]) -> remove_prefix(T, T1);
remove_prefix([], L)         -> L.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
longest_common_prefix(L) -> longest_common_prefix(L, []).
longest_common_prefix(Ls, L) ->
  case have_common_prefix(Ls) of
    {yes, H, Ls1} -> longest_common_prefix(Ls1, [H|L]);
    no            -> reverse(L)
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
have_common_prefix([]) -> no;
have_common_prefix(L)  ->
  case any(fun is_empty_list/1, L) of
    true  -> no;
    false ->
      %% All lists have heads and tails
      Heads = map(fun(I) -> hd(I) end, L),
      H = hd(Heads),
      case all(fun(X) -> hd(X) =:= H end, L) of
        true ->
          Tails = map(fun(I) -> tl(I) end, L),
          {yes, H, Tails};
        false -> no
      end
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
qsort([])        -> [];
qsort([Pivot|T]) ->
  qsort([X || X <- T, X < Pivot]) ++ [Pivot] ++ qsort([X || X <- T, X >= Pivot]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].
