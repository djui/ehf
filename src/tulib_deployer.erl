%%% @doc Deploys recently changed Erlang code by shipping it to a given host.
%%% Files are shipped immediately and as single files.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @todo Replace hardcoded settings with arguments
-module(tulib_deployer).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-compile([nowarn_unused_vars]).

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
        ]).

-include_lib("kernel/include/file.hrl"). %% file_info record

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(TIMEOUT, 5). %% Recheck-Interval in seconds
-define(PATH, "/Users/uwe/dev/klarna/kred").
-define(REMOTE_HOST, "kingkong").
-define(REMOTE_PATH, "/home/uwe/dev/klarna/kred").
-define(E(_Msg), []).

-record(state, {last, tref}).


start()      -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()       -> gen_server:call(?MODULE, stop).
init([])     ->
  {ok, TRef} = timer:send_interval(timer:seconds(?TIMEOUT), check),
  {ok, #state{last = erlang:localtime(), tref = TRef}}.
handle_call(stop, _From, State) -> {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) -> {reply, {error, badrequest}, State}.
handle_cast(_Req, State) -> {noreply, State}.
handle_info(check, State) ->
  Now = erlang:localtime(),
  check(State#state.last, Now), {noreply, State#state{last = Now}};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> {ok, cancel} = timer:cancel(State#state.tref), ok.
code_change(_Vsn, State, _Extra) -> {ok, State}.

check(From, To) ->
  ChangedFiles = traverse(?PATH, {From, To}, []),
  ship_all(ChangedFiles, ?REMOTE_HOST, ?REMOTE_PATH).

traverse(Dir, Timespan, Changes) -> %% depth-first
  case file:list_dir(Dir) of
    {ok, Filenames} -> traverse_(Filenames, Dir, Timespan, Changes);
    {error, Reason} -> ?E({listing, Reason})
  end.

traverse_([],    _,   _,                      Changes) -> Changes;
traverse_([H|T], Dir, Timespan = {From , To}, Changes) ->
  Path = filename:join(Dir, H),
  Changes2 = case file:read_file_info(Path) of
               {ok, #file_info{type = directory}} ->
                 traverse(Path, Timespan, Changes);
               {ok, #file_info{type = regular, mtime = Mtime}} when
                   Mtime > From, Mtime =< To ->
                 Changes ++ [Path];
               {ok, _} ->
                 Changes;
               {error, Reason} ->
                 ?E({read_file_info, Path, Reason}), Changes
             end,
  traverse_(T, Dir, Timespan, Changes2).

ship_all([],    _,        _)        -> ignore;
ship_all(Files, DestHost, DestPath) ->
  Files2 = "\"" ++ string:join(Files, "\" \"") ++ "\"",
  io:format("Shipping ~s... ", [Files2]),
  Cmd = fmt("rsync -a -z ~s ~s:~s", [Files2, DestHost, DestPath]),
  safe_cmd(Cmd).

fmt(Msg, Data) -> lists:flatten(io_lib:format(Msg, Data)).

safe_cmd(Str) -> safe_cmd_(cmd(Str)).
safe_cmd_({0, _}) -> io:format("done.~n");
safe_cmd_({_, S}) -> ?E({rsync, S}).
  
cmd(Str) ->
  Output = os:cmd(Str ++ " ; echo $?"),
  Lines = string:tokens(Output, "\n"),
  [Code|ROutput] = lists:reverse(Lines),
  {Code2, _} = string:to_integer(Code),
  {Code2, lists:reverse(ROutput)}. 
