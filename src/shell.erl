%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(shell).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ error/1
        , exit/0
        , exit/1
        ]).

%% @doc Prints out an error message in colored (red) text. Works only in escript
%% or when using the -oldshell flag.
error(Msg) -> io:format("[\033[01;31mERROR\033[00m] ~s~n", [Msg]), shell:exit(1).

%% @doc Stop an Erlang VM nicely with a the return code 0 and does not allow to
%% continue in the same process.
exit() -> shell:exit(0).

%% @doc Stop an Erlang VM nicely with a given return code and does not allow to
%% continue in the same process.
exit(Code) -> init:stop(Code), receive after infinity -> ok end.
