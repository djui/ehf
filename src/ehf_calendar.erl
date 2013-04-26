%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(ehf_calendar).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ unix_ts/1
        , unix_ts/0
        , unix_ts_to_datetime/1
        ]).

%% @doc Convert an erlang:now/0 timestamp into an unix timestamp.
unix_ts() -> unix_ts(now()).

unix_ts({MegaSecs, Secs, _MicroSecs}) -> MegaSecs*1000000 + Secs;
unix_ts({_Date, _Time}=DateTime) ->
  Base = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
  Secs = calendar:datetime_to_gregorian_seconds(DateTime),
  Secs - Base.

unix_ts_to_datetime(TS) when is_integer(TS) ->
  Base = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
  Secs = Base + TS,
  calendar:gregorian_seconds_to_datetime(Secs).
