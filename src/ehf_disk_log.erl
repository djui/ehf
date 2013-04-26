%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(ehf_disk_log).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ is_valid/1
        , dump/1
        ]).

%% @doc Return true if a given file is a disk_log and not corrupted, otherwise
%% false.
is_valid(File) ->
  case open_safely(File) of
    {ok, Ref}    -> ok = disk_log:close(Ref), true;
    {error, Rsn} -> {false, Rsn}
  end.

%% @doc Return the whole content of a valid disk_log log file.
dump(File) ->
  {ok, Ref} = open_safely(File),
  {_Cont, Term} = disk_log:chunk(Ref, start, infinity),
  Term.

%% @doc Opens a file assuming it's a disk_log safely, in terms of reference name
%% and file modifications.
open_safely(File) ->
  disk_log:open([ {name,   unique_ref()}
                , {file,   File}
                , {repair, false}
                , {mode,   read_only}
                ]).

%% @doc Creates a unique reference to be used as disk_log ref name.
unique_ref() ->
  Seed = erlang:tuple_to_list(erlang:now()),
  Fmt  = lists:flatten(io_lib:format("~p_~p_~p", Seed)),
  erlang:list_to_atom(Fmt).
