%%% @doc OS Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(ehf_os).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ cmd/1
        , cmd/2
        , cmd2/1
        , exit/1
        , exit/2
        ]).

-define(NL, "\n").

%% @doc Executes an OS command and returns the return code along with the
%% standard output text.
cmd(Str) -> cmd(Str, false).

%% @doc Executes an OS command and returns the return code along with the
%% standard output text. If split is true, the return output text is split into
%% lines.
cmd(Str, Split) when is_boolean(Split) ->
  Output = os:cmd(Str ++ " ; echo $?"),
  Lines = string:tokens(Output, ?NL),
  [Code|ROutput] = lists:reverse(Lines),
  {Code2, _} = string:to_integer(Code),
  RROutput = lists:reverse(ROutput),
  case Split of
    true  -> {Code2, RROutput};
    false -> {Code2, string:join(RROutput, ?NL)}
  end.

%% @doc Executes an OS command and returns the return code along with the
%% standard output text. If split is true, the return output text is split into
%% lines.
cmd2(Str) -> %% Note: This can only be applied on idempotent operations!
  Code0 = os:cmd(Str ++ " > /dev/null 2>&1 ; echo $?"),
  {Code, _} = string:to_integer(Code0),
  {Code, os:cmd(Str)}.

%% @doc Shuts down an Erlang VM with a given text and 0 as return code.
exit(S) -> ehf_os:exit(0, S).

%% @doc Shuts down an Erlang VM with a given text and return code.
exit(C, S) -> io:format("~s~n", [S]), halt(C).
