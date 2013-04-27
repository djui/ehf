%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(ehf_supervisor).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-define(MAX_R,    5).
-define(MAX_T,   10).
-define(WAIT,  5000).

-export([ supervision_for_all/1
        , supervision_for_all/3
        , supervision_for_one/1
        , supervision_for_one/3
        , supervision_for_rest/1
        , supervision_for_rest/3
        ]).
-export([ work/1
        , work/2
        , work_then_die/1
        , work_then_die/2
        , work_then_wait/2
        , work_then_wait/3
        , work_temporary/1
        , work_temporary/2
        , work_temporary_then_die/1
        , work_temporary_then_die/2
        , work_temporary_then_wait/2
        , work_temporary_then_wait/3
        , work_transient/1
        , work_transient/2
        , work_transient_then_die/1
        , work_transient_then_die/2
        , work_transient_then_wait/2
        , work_transient_then_wait/3
        , supervise/1
        , supervise/2
        ]).


supervision_for_one(C)       -> supervision_for_one(C, ?MAX_R, ?MAX_T).
supervision_for_one(C, R, T) -> supervision_spec(one_for_one, R, T, C).

supervision_for_all(C)       -> supervision_for_all(C, ?MAX_R, ?MAX_T).
supervision_for_all(C, R, T) -> supervision_spec(one_for_all, R, T, C).

supervision_for_rest(C)       -> supervision_for_rest(C).
supervision_for_rest(C, R, T) -> supervision_spec(rest_for_one, R, T, C).


supervise(M)    -> supervise(M, []).
supervise(M, A) -> child_spec(M, A, permanent, infinity, supervisor).

work(M)    -> work(M, []).
work(M, A) -> work_then_wait(M, A, ?WAIT).

work_then_die(M)    -> work_then_die(M, []).
work_then_die(M, A) -> work_then_wait(M, A, brutal_kill).

work_then_wait(M, T)    -> work_then_wait(M, [], T).
work_then_wait(M, A, T) -> child_spec(M, A, permanent, T, worker).

work_temporary(M)    -> work_temporary(M, []).
work_temporary(M, A) -> work_temporary_then_wait(M, A, ?WAIT).

work_temporary_then_die(M)    -> work_temporary_then_die(M, []).
work_temporary_then_die(M, A) -> work_temporary_then_wait(M, A, brutal_kill).

work_temporary_then_wait(M, T)    -> work_temporary_then_wait(M, [], T).
work_temporary_then_wait(M, A, T) -> child_spec(M, A, temporary, T, worker).

work_transient(M)    -> work_transient(M, []).
work_transient(M, A) -> work_transient_then_wait(M, A, ?WAIT).

work_transient_then_die(M)    -> work_transient_then_die(M, []).
work_transient_then_die(M, A) -> work_transient_then_wait(M, A, brutal_kill).

work_transient_then_wait(M, T)    -> work_transient_then_wait(M, [], T).
work_transient_then_wait(M, A, T) -> child_spec(M, A, transient, T, worker).

supervision_spec(RestartStrategy, MaxR, MaxT, ChildSpecs) ->
  {ok, {{RestartStrategy, MaxR, MaxT}, ChildSpecs}}.

child_spec(Id, Args, Restart, Shutdown, Type) ->
  {Id, {Id, start_link, Args}, Restart, Shutdown, Type, [Id]}. 
