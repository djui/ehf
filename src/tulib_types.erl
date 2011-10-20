%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @todo Check if erl_lint.erl can help out
-module(tulib_types).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ check/2
        ]).

%%% Code =======================================================================
%% @doc Runs type-check on data structure against remote/local type definition.
check(V, {M,N}) -> check(V, M, N);
check(V, MN) when is_atom(MN) ->
  case string:tokens(atom_to_list(MN), ":") of
    [Mod, Name] -> check(V, list_to_atom(Mod), list_to_atom(Name));
    [Name]      -> check(V, ?MODULE,           list_to_atom(Name))
  end.

check(Value, TModule, TName) ->
  Types = from_module(TModule),
  Type  = resolve(TName, Types),
  I     = from_form(Type),
  T     = from_term(Value),
  erl_types:t_is_instance(T, I).

%%% Internals ------------------------------------------------------------------
resolve(Name, Types) -> traverse(form(Name, Types), Types).

traverse({type,Line,Type,SubTypes}, Types) ->
  case lists:member(Type, builtin_types() ++ var_arity_types()) of
    true  -> ResTypes = lists:map(fun(T) -> traverse(T, Types) end, SubTypes),
             {type,Line,Type,ResTypes};
    false -> resolve(Type, Types)
  end;
traverse({_BuiltinType,_Line,_Value}=TypeForm, _Types) -> TypeForm.

from_module(Module) ->
  {ok,AC}    = dialyzer_utils:get_abstract_code_from_beam(code:which(Module)),
  {ok,Types} = dialyzer_utils:get_record_and_type_info(AC),
  Types.

from_form(Type) -> catch erl_types:t_from_form(Type).

from_term(Term) -> erl_types:t_from_term(Term).

form(Name, Types) -> {_,TypeForm,[]} = dict:fetch({type,Name}, Types), TypeForm.

%% @doc Taken from erl_lint:default_types/0.
builtin_types() ->
  [ any
  , array
  , arity
  , atom
  , binary
  , bitstring
  , bool
  , boolean
  , byte
  , char
  , dict
  , digraph
  , float
  , 'fun'
  , function
  , gb_set
  , gb_tree
  , identifier
  , integer
  , iodata
  , iolist
  , list
%%, matchstate
  , maybe_improper_list
  , mfa
  , module
  , neg_integer
  , nil
  , no_return
  , node
  , non_neg_integer
  , none
  , nonempty_list
  , nonempty_improper_list
  , nonempty_maybe_improper_list
  , nonempty_string
  , number
%%, opaque
  , pid
  , port
  , pos_integer
  , queue
  , range
  , reference
%%, remote
  , set
  , string
  , term
  , timeout
%%, tuple_set
%%, var
  ].

%% @doc Taken from erl_lint:is_var_arity_type/1.
var_arity_types() ->
  [ tuple
  , product
  , union
  , record
  ].
