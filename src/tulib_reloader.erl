%%% @copyright 2007 Mochi Media, Inc.
%%% @author Matthew Dempsky <matthew@mochimedia.com>
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%%
%%% @doc Erlang module for automatically reloading modified modules during
%%% development.
-module(tulib_reloader).

-author("Matthew Dempsky <matthew@mochimedia.com>").
-author("Uwe Dauenrheim <uwe@dauernheim.net>").

-behaviour(gen_server).

-export([ start/0
        , start_link/0
        , stop/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , reload_modules/1
        , all_changed/0
        , is_changed/1
        ]).

-include_lib("kernel/include/file.hrl"). %% file_info record

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(TIMEOUT, 1). %% Check for changes repeat cycle

-record(state, {last, tref}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SERVER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()      -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()       -> gen_server:call(?MODULE, stop).
init([])     ->
  {ok, TRef} = timer:send_interval(timer:seconds(?TIMEOUT), doit),
  {ok, #state{last = erlang:localtime(), tref = TRef}}.
handle_call(stop, _From, State) -> {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) -> {reply, {error, badrequest}, State}.
handle_cast(_Req, State) -> {noreply, State}.
handle_info(doit, State) ->
  Now = erlang:localtime(),
  doit(State#state.last, Now),
  {noreply, State#state{last = Now}};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> {ok, cancel} = timer:cancel(State#state.tref), ok.
code_change(_Vsn, State, _Extra) -> {ok, State}.

doit(From, To) ->
  [case file:read_file_info(Filename) of
     {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To -> reload(Module);
     {ok, _} -> unmodified;
     {error, enoent} -> gone;
     {error, Reason} -> io:format("Error reading ~s's file info: ~p~n", [Filename, Reason]), error
   end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

reload(Module) ->
  io:format("Reloading ~p ...", [Module]),
  code:purge(Module),
  case code:load_file(Module) of
    {module, Module} ->
      io:format(" ok.~n"),
      case erlang:function_exported(Module, test, 0) of
        true ->
          io:format("Calling ~p:test() ...", [Module]),
          case catch Module:test() of
            ok -> io:format(" ok.~n"), reload;
            Reason -> io:format(" fail: ~p.~n", [Reason]), reload_but_test_failed
          end;
        false -> reload
      end;
    {error, Reason} -> io:format(" fail: ~p.~n", [Reason]), error
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reload_modules(Modules) -> [begin code:purge(M), code:load_file(M) end || M <- Modules].
all_changed() -> [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].
is_changed(M) ->
  try module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
  catch _:_ -> false
  end.
module_vsn({M, Beam, _Fn})    -> {ok, {M, Vsn}} = beam_lib:version(Beam), Vsn;
module_vsn(L) when is_list(L) ->
  {_, Attrs} = lists:keyfind(attributes, 1, L),
  {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
  Vsn.
