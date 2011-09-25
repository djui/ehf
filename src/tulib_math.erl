%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(tulib_math).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ odd/1
        , odds_and_evens/1
        , sum/1
        ]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
odd(X) ->
  case X band 1 of
    1 -> true;
    0 -> false
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
odds_and_evens(L) ->
  Odds  = [X || X <- L, odd(X)],
  Evens = [X || X <- L, odd(X) =/= false],
  {Odds, Evens}.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
sum(L) -> sum(L, 0).
sum([], N)    -> N;
sum([H|T], N) -> sum(T, H+N).
