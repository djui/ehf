%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(os).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ exitcode/1
        ]).

%% @doc Executes an OS command and returns the error code instead of the 
%% standard output text.
exitcode(Cmd) ->
  CodeStr = os:cmd(Cmd ++ " > /dev/null 2>&1 ; echo $?"),
  {Code, _} = string:to_integer(CodeStr),
  Code.
