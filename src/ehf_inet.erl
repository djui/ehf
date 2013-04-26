%%% @doc INet Helper functions.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(ehf_inet).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([ node2host/1
        , node2ip/1
        , node2ipv6/1
        ]).


%% @doc Returns the host of a given node name.
node2host(Node) ->
  Node2 = atom_to_list(Node),
  [_Name, Host] = string:tokens(Node2, "@"),
  Host.

%% @doc Returns the IPv4 address of a given node name.
node2ip(Node) ->
  Host = node2host(Node),
  case inet:getaddr(Host, inet) of
    {ok, Address} -> Address;
    {error, _}    -> undefined
  end.

%% @doc Returns the IPv6 address of a given node name.
node2ipv6(Node) ->
  Host = node2host(Node),
  case inet:getaddr(Host, inet6) of
    {ok, Address} -> Address;
    {error, _}    -> undefined
  end.
