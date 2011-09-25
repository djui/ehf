%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(tulib_assert).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-define(IMPLIES(A, B), (not (A)) orelse (B)).
-define(IF(Bool, A   ), if Bool -> A; true -> undefined end).
-define(IF(Bool, A, B), if Bool -> A; true -> B         end).
