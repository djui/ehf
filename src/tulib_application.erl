%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Basho Technologies
-module(tulib_application).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Basho Technologies").

-export([ ensure_started/1
        , get_env/2
        , get_env/3
        , priv_dir/0
        , priv_file/1
        , priv_file/2
        ]).

%% @doc Ensure that a given application is started.
%% @credits Basho Technologies
ensure_started(Application) ->
  ensure_started(Application, application:start(Application)).

ensure_started(_Application, ok) ->
  ok;
ensure_started(_Application, {error, {already_started, _Application}}) ->
  ok;
ensure_started(Application, {error, {not_started, Dependency}}) ->
  ok = ensure_started(Dependency),
  ensure_started(Application);
ensure_started(Application, {error, Reason}) ->
  erlang:error({app_start_failed, Application, Reason}).

%% @doc Get environment values given a key from the current application,
%% otherwise use a default value.
get_env(Key, Default) ->
  case application:get_env(Key) of
    {ok, Defined} -> Defined;
    undefined     -> Default
  end.

%% @doc Get environment values given a key from a given application, otherwise
%% use a default value.
get_env(Application, Key, Default) ->
  case application:get_env(Application, Key) of
    {ok, Defined} -> Defined;
    undefined     -> Default
  end.

%% @doc Returns the priv directory path for the current application.
priv_dir() ->
  {ok, App} = application:get_application(),
  code:priv_dir(App).

%% @doc Returns the path of a file residing in the priv directory path for the
%% current application.
priv_file(File) ->
  filename:join([priv_dir(), File]).

%% @doc Returns the path of a file residing in the priv directory path for a
%% given application.
priv_file(Application, File) ->
  filename:join([code:priv_dir(Application), File]).
