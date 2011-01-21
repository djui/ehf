%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(process).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ safe/1
        , spawn_monitor/3
        , monitor/2
        , keep_alive/2
        , make_global/2
        , on_exit/2
        , every/3
        , pmap/2
        , gather/2
        , pmap1/2
        , gather1/3
        ]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
safe(Fun) ->
  case (catch Fun()) of
    {'EXIT', Why} -> {error, Why};
    Other         -> Other
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
spawn_monitor(_, false, Fun)   -> spawn(Fun);
spawn_monitor(Term, true, Fun) -> spawn(fun() -> starter(Term, Fun) end).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
starter(Term, Fun) ->
  S = self(),
  io:format("process:~p started at:~p ~p~n", [self(), erlang:now(), Term]),
  Monitor = spawn_link(fun() -> monitor(Term, S) end),
  receive
    {Monitor, ready} ->
      Fun()
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
monitor(Term, Parent) ->
  process_flag(trap_exit, true),
  Parent ! {self(), ready},
  receive
    {'EXIT', Parent, Why} ->
      io:format("process:~p dies at:~p ~p reason:~p~n",
                [self(), erlang:now(), Term, Why])
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
keep_alive(Name, Fun) ->
  register(Name, Pid = spawn(Fun)),
  on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).

%% @doc Checks if there is a global process with the
%% registered name Name. If there is no process it spawns a process to
%% evaluate Fun() and registers it with the name Name.
%% @copyright Programming Erlang - The Pragmatic Bookshelf
make_global(Name, Fun) ->
  S = self(),
  Pid = spawn(fun() -> make_global(S, Name, Fun) end),
  receive
    {Pid, Reply} ->
      Reply
  end.
make_global(Parent, Name, Fun) ->	
  case (catch register(Name, self())) of
    true -> Fun();
    _ -> true
  end,
  Parent ! {self(), ok}.

%% @doc links to Pid. If Pid dies with reason Why then Fun(Why) is evaluated
%% @copyright Programming Erlang - The Pragmatic Bookshelf
on_exit(Pid, Fun) ->
  spawn(fun() -> 
            process_flag(trap_exit, true),
            link(Pid),
            receive
              {'EXIT', Pid, Why} -> Fun(Why)
            end
        end).

%% @doc links to Pid then every Time Fun() is evaluated. If Pid exits, this 
%% process stops.
%% @copyright Programming Erlang - The Pragmatic Bookshelf
every(Pid, Time, Fun) ->
  spawn(fun() ->
            process_flag(trap_exit, true),
            link(Pid),
            every_loop(Pid, Time, Fun)
        end).
every_loop(Pid, Time, Fun) ->
  receive
    {'EXIT', Pid, _Why} ->
      true
  after Time ->
      Fun(),
      every_loop(Pid, Time, Fun)
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
pmap(F, L) -> 
  S = self(),
  %% make_ref() returns a unique reference
  %%   we'll match on this later
  Ref = erlang:make_ref(), 
  Pids = map(fun(I) -> 
                 spawn(fun() -> do_f(S, Ref, F, I) end)
             end, L),
  %% gather the results
  gather(Pids, Ref).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
do_f(Parent, Ref, F, I) ->
  Parent ! {self(), Ref, (catch F(I))}.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
gather([Pid|T], Ref) ->
  receive
    {Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
  end;
gather([], _) -> [].

%% @copyright Programming Erlang - The Pragmatic Bookshelf
pmap1(F, L) -> 
  S = self(),
  Ref = erlang:make_ref(),
  foreach(fun(I) -> 
              spawn(fun() -> do_f1(S, Ref, F, I) end)
          end, L),
  %% gather the results
  gather1(length(L), Ref, []).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
do_f1(Parent, Ref, F, I) ->
  Parent ! {Ref, (catch F(I))}.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
gather1(0, _, L) -> L;
gather1(N, Ref, L) ->
  receive
    {Ref, Ret} -> gather1(N-1, Ref, [Ret|L])
  end.
