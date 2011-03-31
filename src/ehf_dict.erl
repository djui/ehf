%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(dict2).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ merge_kv/1
        , lookup/2
        ]).

%% @doc Take a association list of {Key, Val} where Key can occure
%% More than once and make it into a list {Key, [Val]} where
%% each Key occurs only once
%% @copyright Programming Erlang - The Pragmatic Bookshelf
merge_kv(KV) ->  merge_kv(KV, dict:new()).
merge_kv([{Key,Val}|T], D0) ->
  case dict:find(Key, D0) of
    {ok, L} -> merge_kv(T, dict:store(Key, [Val|L], D0));
    error   -> merge_kv(T, dict:store(Key, [Val], D0))
  end;
merge_kv([], D) -> dict:to_list(D).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
lookup(Key, [{Key,Val}|_]) -> {ok, Val};
lookup(Key, [_|T])         -> lookup(Key, T);
lookup(_, [])              -> error.
