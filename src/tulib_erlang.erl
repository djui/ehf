%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(tulib_erlang).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ deliberate_error/1
        , e/2
        , for/3
        , fun_to_mfa/1
        , len/1
        , otp_version/0
        , p/1
        , pid/1
        , pid/2
        ]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
deliberate_error(A) ->
  bad_function(A, 12),
  lists:reverse(A).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
bad_function(A, _) ->
  {ok, Bin} = file:open({abc,123}, A),
  erlang:list_to_binary(Bin).

%% @doc Get element N of tuple or list.
e(N, T) when is_list(T)  -> lists:nth(N, T);
e(N, T) when is_tuple(T) -> erlang:element(N, T).

%% @doc Return the amount of elements in a list, tuple, or the amount of digits
%% in an integer.
len(L) when is_list(L)    -> erlang:length(L);
len(T) when is_tuple(T)   -> erlang:size(T);
len(D) when is_integer(D) -> len(erlang:integer_to_list(D)).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I)|for(I+1, Max, F)].

%% @doc Convert a fun (lambda) into a tuple with module name, fun name, and
%% arity count.
fun_to_mfa(Fun) when is_function(Fun) ->
  { element(2, erlang:fun_info(Fun, module))
  , element(2, erlang:fun_info(Fun, name  ))
  , element(2, erlang:fun_info(Fun, arity ))
  }.

%% @doc Return the OTP version as tuple: {Major, Cycle, Minor}.
otp_version() ->
  try
    [$R | Version]  = erlang:system_info(otp_release),
    {Major, Build}  = string:to_integer(Version),
    [Cycle | Minor] = Build,
    {Minor2, _}     = string:to_integer(Minor),
    {Major, [Cycle], Minor2}
  catch
    _:_ -> undefined
  end.

%% @doc Short cut to pretty-print a term.
p(Data) -> io:format("~p~n", [Data]).

%% @doc Create pid variable type.
pid(I2,I3)                    -> pid({I2,I3}).
pid({I1,I2,I3})               -> c:pid(I1,I2,I3);
pid({I2,I3})                  -> pid({0,I2,I3});
pid(Pid)  when is_pid(Pid)    -> Pid;
pid(Atom) when is_atom(Atom)  -> whereis(Atom);
pid(I2)   when is_integer(I2) -> pid({0,I2,0});
pid(Str)  when hd(Str)==$<    -> list_to_pid(Str);
pid(Str)  when is_list(Str)   -> pid("<"++Str++">").
