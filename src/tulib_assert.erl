%%% @doc Composable error handling.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Jakob Siever <canned.primat.es@gmail.com>
-module(tulib_assert).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-auhtor("Jakob Siever <canned.primat.es@gmail.com>").

%%% Exports ====================================================================
-export([ lift/1
        , unlift/1
        ]).

%%% Exports ====================================================================
-define(IMPLIES(A, B), (not (A)) orelse (B)).
-define(IF(Bool, A   ), if Bool -> A; true -> undefined end).
-ifdef(OTP_R14).
-define(IF(Bool, A, B), if Bool -> A; true -> B         end).
-endif.

-define(is_dict(D), (is_tuple(D) andalso element(1, D) =:= dict)).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
lift(F) ->
  try F() of
      ok                          -> ok;
      {ok,    Res}                -> {ok,    Res};
      error                       -> error;
      {error, Rsn}                -> {error, Rsn};
      Res                         -> {ok,    Res}
  catch
    _:error                       -> error;
    _:{error,       Rsn         } -> {error, Rsn};
    _:{badmatch,    error       } -> error;
    _:{badmatch,    {error, Rsn}} -> {error, Rsn};
    _:{case_clause, error       } -> error;
    _:{case_clause, {error, Rsn}} -> {error, Rsn};
    _:Exn                         -> {error, Exn}
  end.

unlift(F) ->
  case F() of
    ok           -> ok;
    {ok,    Res} -> Res;
    error        -> throw(error);
    {error, Rsn} -> throw({error, Rsn});
    Res          -> Res
  end.
