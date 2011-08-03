%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(ehf_beam_lib).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ ast/1
        , source/1
        , print_source/1
        ]).

%% @doc Extract the abstract code (abstract syntax tree) of a module from a beam
%% file compiled with debug_info.
ast(Module) ->
  BeamFile = code:which(Module),
  AbstractCode = beam_lib:chunks(BeamFile, [abstract_code]),
  {ok, {Module, [{abstract_code, {raw_abstract_v1, AC}}]}} = AbstractCode,
  erl_syntax:form_list(AC).

%% @doc Extract the source code of a module from a beam file compiled with
%% debug_info.
source(Module) -> erl_prettypr:format(ast(Module)).

%% @doc Print source code to shell.
print_source(Module) -> io:format("~s~n", [source(Module)]).
