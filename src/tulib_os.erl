%%% @doc OS Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(tulib_os).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ cmd/1
        , cmd/2
        , cmd2/1
        , cmd2/2
        , cmd3/1
        , exit/1
        , exit/2
        ]).

-define(NL, io_lib:format("~n")).

%% @doc Running an external command as port along with the return code.
cmd(Cmd) -> cmd(Cmd, []).

cmd(Cmd, Args) ->
  Port = erlang:open_port({spawn_executable, Cmd}, [exit_status]),
  try os_cmd_loop(Port, [])
  catch _C:R -> {error, R}
  after erlang:close_port(Port)
  end.

cmd_loop(Port, Data) ->
  receive
    {Port, {data, Data}}          -> os_cmd_loop(Port, Data ++ NewData);
    {Port, {exit_status, Status}} -> {ok, Status, Data};
    {'EXIT', Port, Reason}        -> {error, Reason}
  end.

%% @doc Executes an OS command and returns the return code along with the
%% standard output text.
cmd2(Str) -> cmd2(Str, false).

%% @doc Executes an OS command and returns the return code along with the
%% standard output text. If split is true, the return output text is split into
%% lines.
cmd2(Str, Split) when is_boolean(Split) ->
  Output         = os:cmd(Str ++ " ; echo $?"),
  Lines          = string:tokens(Output, ?NL),
  [Code|ROutput] = lists:reverse(Lines),
  {Code2, _}     = string:to_integer(Code),
  RROutput       = lists:reverse(ROutput),
  case Split of
    true  -> {Code2, RROutput};
    false -> {Code2, string:join(RROutput, ?NL)}
  end.

%% @doc Executes an OS command and returns the return code along with the
%% standard output text. If split is true, the return output text is split into
%% lines.
cmd3(Str) -> %% Note: This can only be applied on idempotent operations!
  Code0 = os:cmd(Str ++ " > /dev/null 2>&1 ; echo $?"),
  {Code, _} = string:to_integer(Code0),
  {Code, os:cmd(Str)}.

%% @doc Shuts down an Erlang VM with a given text and 0 as return code.
exit(S) -> tulib_os:exit(0, S).

%% @doc Shuts down an Erlang VM with a given text and return code.
exit(C, S) -> io:format("~s~n", [S]), halt(C).
