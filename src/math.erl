%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(math).

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
  Odds  = [X || X <- L, (X rem 2) =:= 1], 
  Evens = [X || X <- L, (X rem 2) =:= 0],
  {Odds, Evens}.
odds_and_evens_acc(L) -> odds_and_evens_acc(L, [], []).
odds_and_evens_acc([H|T], Odds, Evens) ->
  case (H rem 2) of
    1 -> odds_and_evens_acc(T, [H|Odds], Evens);
    0 -> odds_and_evens_acc(T, Odds, [H|Evens])
  end;
odds_and_evens_acc([], Odds, Evens) -> {Odds, Evens}.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
sum(L) -> sum(L, 0).
sum([], N)    -> N;
sum([H|T], N) -> sum(T, H+N).
