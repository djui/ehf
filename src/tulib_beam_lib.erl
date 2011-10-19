%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
%%% @author Erlang/OTP source
-module(tulib_beam_lib).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ ast/1
        , beam/1
        , cflags/0
        , cflags/1
        , dflags/1
        , export_all/1
        , make_term/1
        , print_source/1
        , source/1
        , src/1
        ]).

%% @doc Return file path location to source file.
src(M) -> proplists:get_value(source, M:module_info(compile)).

%% @doc Return file path location to beam file.
beam(Module) -> code:which(Module).

%% @doc Extract the abstract code (abstract syntax tree) of a module from a beam
%% file compiled with debug_info.
ast(Module) ->
  BeamFile = code:which(Module),
  AbstractCodeChunk = beam_lib:chunks(BeamFile, [abstract_code]),
  AbstractCode = case AbstractCodeChunk of
                   {ok, {Module, [{abstract_code, {raw_abstract_v1, AC}}]}} ->
                     AC;
                   {ok, {Module, [{abstract_code, no_abstract_node}]}} ->
                     throw({no_abstract_code, maybe_compiled_without_debug});
                   _ ->
                     throw(unknown_error)
                 end,
  erl_syntax:form_list(AbstractCode).

%% @doc Extract the source code of a module from a beam file compiled with
%% debug_info.
source(Module) -> erl_prettypr:format(ast(Module)).

%% @doc Print source code to shell.
print_source(Module) -> io:format("~s~n", [source(Module)]).

%% @doc Recompiles M with export_all without access to the source.
export_all(M) ->
  case code:which(M) of
    non_existing -> no_such_module;
    F -> 
      {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(F, [abstract_code]),
      {ok,_,B} = compile:forms(AC, [export_all]),
      code:soft_purge(M),
      code:load_binary(M, "", B)
  end.

%% @doc Return all compile defines of a given module.
dflags(M) ->
  lists:filter(fun({d,_,_}) -> true;
                  ({d,_})   -> true;
                  (_)       -> false
               end, cflags(M)).

%% @doc Return all compile attributes of a given module.
cflags(M) ->
  Flags = M:module_info(compile),
  proplists:get_value(options, Flags).

%% @doc Return all environment compiler options.
%% Stripped version of compile:env_default_opts/0.
cflags() ->
  Opts0 = os:getenv("ERL_COMPILER_OPTIONS"),
  {ok,Tokens,_} = erl_scan:string(Opts0),
  {ok, Term} = erl_parse:parse_term(Tokens ++ [{dot,1}]),
  Term.

%% @doc Makes an Erlang term given a string.
%% @reference Taken from std_lib/src/erl_compile.erl
make_term(Str) -> 
  case erl_scan:string(Str) of 
    {ok,Tokens,_} ->
      case erl_parse:parse_term(Tokens ++ [{dot,1}]) of
        {ok, Term}            -> Term;
        {error, {_,_,Reason}} ->
          io:format("~s: ~s~n", [Reason, Str]),
          throw(error)
      end;
    {error,{_,_,Reason},_} ->
      io:format("~s: ~s~n", [Reason, Str]),
      throw(error)
  end.
