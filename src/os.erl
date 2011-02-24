%%% @doc OS Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(os).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ cmd/1
        ]).

-define(NL, "\n").

%% @doc Executes an OS command and returns the return code along with the
%% standard output text.
cmd(Str) ->
  Output = os:cmd(Str ++ " ; echo $?"),
  Lines = string:tokens(Output, ?NL),
  [Code|ROutput] = lists:reverse(Lines),
  {Code2, _} = string:to_integer(Code),
  Output2 = string:join(lists:reverse(ROutput), ?NL),
  {Code2, Output2}.
