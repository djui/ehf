%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(tulib_application).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ ensure_started/1
        , get_env/2
        , get_env/3
        , priv_dir/0
        , priv_file/1
        ]).

%% @doc Ensure that a given application is started.
ensure_started(Application) ->
  case application:start(Application) of
    ok ->
      ok;
    {error, {already_started, Application}} ->
      ok
  end.

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
