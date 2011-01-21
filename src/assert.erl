%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(assert).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-define(implies(A, B), (not (A)) orelse (B)).
-define(if(Bool, A   ), if Bool -> A; true -> undefined end).
-define(if(Bool, A, B), if Bool -> A; true -> B         end).
