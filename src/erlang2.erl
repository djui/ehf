%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(erlang2).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ for/3
        , deliberate_error/1
        , extract_sourcecode/1
        ]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I)|for(I+1, Max, F)].

%% @copyright Programming Erlang - The Pragmatic Bookshelf
deliberate_error(A) ->
  bad_function(A, 12),
  lists:reverse(A).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
bad_function(A, _) ->
  {ok, Bin} = file:open({abc,123}, A),
  binary_to_list(Bin).

%% @doc Return the OTP major and minor version.
otp_version() ->
  try
    ["R" | OTP] = erlang:system_info(otp_release),
    {Major, Build} = string:to_integer(OTP),
    [Cycle | Minor] = Build,
    Minor2 = string:to_integer(Minor),
    {Major, Cycle, Minor2}
  catch
    _:_ -> undefined
  end.
