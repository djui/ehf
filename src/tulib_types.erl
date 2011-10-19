%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(tulib_types).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ complies/2
        ]).

%%% Code =======================================================================
complies(Value, {Module, Name}) ->
  Types = extract_types(Module),
  Type  = proplists:get_value(Name, Types),
  I = erl_types:t_from_form(Type),
  T = erl_types:t_from_term(Value),
  erl_types:t_is_instance(T, I).

%%% Internals ------------------------------------------------------------------
extract_types(M) ->
  {tree, form_list, _, AS} = tulib_beam_lib:ast(M),
  TypeFun = fun({attribute, _, type, {Name, Form, []}}, Acc) ->
                [{Name, Form}|Acc];
               (_, Acc) ->
                Acc
            end,
  lists:foldl(TypeFun, [], AS).
