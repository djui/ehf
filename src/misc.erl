%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(misc).

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

%% @doc Extract the source code from a beam file compiled with debug_info.
extract_sourcecode(Module) ->
  BeamFile = code:which(Module),
  AbstractCode = beam_lib:chunks(BeamFile, [abstract_code]),
  {ok, {Module, [{abstract_code, {raw_abstract_v1, AC}}]}} = AbstractCode,
  Code = erl_prettypr:format(erl_syntax:form_list(AC)),
  io:format("~s~n", [Code]).
