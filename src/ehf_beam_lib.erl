%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(ehf_beam_lib).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ extract_sourcecode/1
        ]).

%% @doc Extract the source code from a beam file compiled with debug_info.
extract_sourcecode(Module) ->
  BeamFile = code:which(Module),
  AbstractCode = beam_lib:chunks(BeamFile, [abstract_code]),
  {ok, {Module, [{abstract_code, {raw_abstract_v1, AC}}]}} = AbstractCode,
  Code = erl_prettypr:format(erl_syntax:form_list(AC)),
  io:format("~s~n", [Code]).
