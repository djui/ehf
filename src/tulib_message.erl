%%% @doc Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
%%% @author Programming Erlang - The Pragmatic Bookshelf
-module(tulib_message).

-author("Uwe Dauernheim <uwe@dauernheim.net>").
-author("Programming Erlang - The Pragmatic Bookshelf").

-export([ flush_msg_queue/0
        , priority_receive/0
        , rpc/2
        ]).

%% @copyright Programming Erlang - The Pragmatic Bookshelf
flush_msg_queue() ->
  receive
    _Any -> flush_msg_queue()
  after
    0 -> true
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
priority_receive() ->
  receive
    {alarm, X} -> {alarm, X}
  after 0 ->
      receive
        Any -> Any
      end
  end.

%% @copyright Programming Erlang - The Pragmatic Bookshelf
rpc(Pid, Msg) ->
  Pid ! {self(), Msg},
  receive
    {Pid, Reply} -> Reply
  end.
