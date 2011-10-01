%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(tulib_date).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ unix_timestamp/1 ]).

%% @doc 
unix_timestamp({MegaSecs, Secs, _MicroSecs}) -> MegaSecs*1000000 + Secs;
unix_timestamp({_Date, _Time}=DateTime) ->
  Secs = calendar:datetime_to_gregorian_seconds(DateTime),
  Base = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  Secs - Base.
