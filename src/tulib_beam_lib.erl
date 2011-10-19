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
        , print_fun/0
        , print_fun/1
        , print_source/1
        , source/1
        , src/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
                   {ok, {Module, [{abstract_code, no_abstract_code}]}} ->
                     throw({no_abstract_code, maybe_compiled_without_debug});
                   {error, _} = E ->
                     throw(E)
                 end,
  erl_syntax:form_list(AbstractCode).

%% @doc Extract the source code of a module from a beam file compiled with
%% debug_info.
source(Module) -> erl_prettypr:format(ast(Module)).

%% @doc We want to have a set of instructions encapsulated into a thunk:
%%   Instructions = ...,
%%   Fun = {'fun',1,{clauses,Instructions}}.
%% And then output this thunk as a human-readable string:
%%   fun() -> ... end
print_fun() ->
  {ok,Form,_} = io:parse_erl_exprs('fun-shell> '),
  print_form(Form).

print_fun(FunStr0) when is_list(FunStr0) ->
  FunStr = case lists:last(FunStr0) of 
             "." -> FunStr0;
             _   -> FunStr0++"."
           end,
  {ok,Tokens,_} = erl_scan:string(FunStr),
  {ok,Form}     = erl_parse:parse_exprs(Tokens),
  print_form(Form);
print_fun(Fun) when is_function(Fun) ->
  FunInfo = erlang:fun_info(Fun),
  Type = proplists:get_value(type,   FunInfo),
  Env  = proplists:get_value(env,    FunInfo),
  M    = proplists:get_value(module, FunInfo),
  F    = proplists:get_value(name,   FunInfo),
  A    = proplists:get_value(arity,  FunInfo),
  case Type of
    external ->
      try
        Form = find_fun(M, F, A),
        print_form(Form)
      catch T:R -> throw({T,R})
      end;
    local ->
      case Env of
        []  ->
          {F2,A2,I} = closure_name_to_fun_location(F, A),
          try
            Form = find_fun(M, F2, A2, I),
            print_form(Form)
          catch T:R -> throw({T,R})
          end;
        Env ->
          [[],_,_,Form0] = Env,
          Form = [{'fun',1,{clauses,Form0}}], %% Add a default fun header
          print_form(Form)
      end
  end.

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

%%% Internals ------------------------------------------------------------------
%% @doc Take an AST form and print it as a source code string.
print_form(Form) ->
  Source = convert_form(Form),
  io:format("~s~n", [Source]).
  
%% @doc Take an AST form and converts it into a source code string.
convert_form(Form) ->
  Syntax = erl_syntax:form_list(Form),
  erl_prettypr:format(Syntax).
  
find_fun(M, F, A) ->
  AC = get_ast(M),
  get_fun_form(AC, F, A).

find_fun(M, F, A, I) ->
  AC = get_ast(M),
  FunForm = get_fun_form(AC, F, A),
  get_indexed_fun_form(FunForm, I).

get_ast(Module) ->
  BeamFile = code:which(Module),
  AbstractCode = beam_lib:chunks(BeamFile, [abstract_code]),
  {ok,{Module,[{abstract_code,{raw_abstract_v1,AC}}]}} = AbstractCode,
  AC.

get_fun_form(AC, F, A) ->
  [Form2] = [Form || {function,_,F0,A0,Form} <- AC, F0 =:= F, A0 =:= A],
  Form2.

%% @doc This needs big improvements to be more generalized (if it is possible at
%% all to evaluate the function body and find the currect declaration).
get_indexed_fun_form(FunForm, I) ->
  [{clause,_,[],[],Body}] = FunForm,
  As = lists:foldl(fun({_,_,_,A={'fun',_,_}}, Acc) -> Acc++[A]; %% assigned
                      ({_,_,_,As}, Acc) when is_list(As) ->     %% argument
                       Acc++[A || A={'fun',_,_} <- As];
                      (_, Acc) -> Acc                           %% none
                   end, [], Body),
  [lists:nth(I+1, As)].

%% @doc Convert a closure name like '-print_fun_test/0-fun-0-' into a fun name
%% like 'print_fun_test' by looking it up in the module's beam code. Pattern
%% follows the string creation of compiled fun names as defined in
%% compiler/sys_pre_expand:new_fun_name/1.
closure_name_to_fun_location(Closure, _A) ->
  Closure2 = erlang:atom_to_list(Closure),
  Pattern = "^-(.*)/([0-9]+)-fun-([0-9+])-$",
  {match, [_,F,A,I]} = re:run(Closure2, Pattern, [{capture, all, list}]),
  {erlang:list_to_atom(F),erlang:list_to_integer(A),erlang:list_to_integer(I)}.

%%% Tests ======================================================================
-ifdef(TEST).

print_fun_test() ->
  io:format("@@@ Test: String~n"),
  print_fun("fun() -> foo end."),
  
  io:format("@@@ Test: Remote compiled~n"),
  print_fun(fun erlang:nodes/0),
  
  io:format("@@@ Test: Local compiled~n"),
  print_fun(fun() -> foo end),
  
  io:format("@@@ Test: Local interpreted~n"),
  io:format("Need to run interactively.~n"),
  %% print_fun(fun() -> foo end),
  
  io:format("@@@ Test: Shell~n"),
  io:format("Need to run interactively.~n"),
  print_fun(),
  ok.

-endif.
