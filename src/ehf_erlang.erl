%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(ehf_erlang).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ for/3
        , deliberate_error/1
        , otp_version/0
        , fun_to_mfa/1
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

%% @doc Return the OTP version as tuple: {Major, Cycle, Minor}.
otp_version() ->
  try
    [$R | Version] = erlang:system_info(otp_release),
    {Major, Build} = string:to_integer(Version),
    [Cycle | Minor] = Build,
    {Minor2, _} = string:to_integer(Minor),
    {Major, [Cycle], Minor2}
  catch
    _:_ -> undefined
  end.

%% @doc Convert a fun (lambda) into a tuple with module name, fun name, and
%% arity count.
fun_to_mfa(Fun) when is_function(Fun) ->
  { element(2,erlang:fun_info(Fun, module))
  , element(2,erlang:fun_info(Fun, name))
  , element(2,erlang:fun_info(Fun, arity))
  }.
