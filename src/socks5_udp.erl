-module(socks5_udp).

-export([messages/1,
  connect/4, connect/5,
  recv/2, recv/3,
  send/4,
  setopts/2,
  controlling_process/2,
  peername/1,
  close/1,
  shutdown/2,
  remove_header/1,
  sockname/1]).

-define(TIMEOUT, infinity).

-type socks5_socket() :: {inet:socket(), inet:socket(), tuple(), integer()}.
-export_type([socks5_socket/0]).

%% @doc Atoms used to identify messages in {active, once | true} mode.
messages({_, _}) ->
  {tcp, tcp_closed, tcp_error}.


connect(Host, Port, UdpSocket, Opts) ->
  connect(Host, Port, UdpSocket, Opts, infinity).


connect(Host, Port, UdpSocket, Opts, Timeout) when (is_list(Host) orelse is_tuple(Host)), is_integer(Port),
                                        (Timeout =:= infinity orelse is_integer(Timeout)) ->
  %% get the proxy host and port from the options
  ProxyHost = proplists:get_value(socks5_host, Opts),
  ProxyPort = proplists:get_value(socks5_port, Opts),

  %% filter connection options
  AcceptedOpts =  [linger, nodelay, send_timeout,
    send_timeout_close, raw, inet6],
  BaseOpts = [binary, {active, false}, {packet, 0}, {keepalive,  true},
    {nodelay, true}],
  ConnectOpts = filter_options(Opts, AcceptedOpts, BaseOpts),

  %% connect to the socks 5 proxy
  case gen_tcp:connect(ProxyHost, ProxyPort, ConnectOpts, Timeout) of
    {ok, Socket} ->
      case do_handshake(Socket, Host, Port, Opts) of
        {ok, UdpHost, UdpPort} ->
          {ok, {Socket, UdpSocket, UdpHost, UdpPort}};
        Error ->
          gen_tcp:close(Socket),
          Error
      end;
    Error ->
      Error
  end.

%% @doc filter a proplists and only keep allowed keys
-spec filter_options([{atom(), any()} | {raw, any(), any(), any()}],
  [atom()], Acc) -> Acc when Acc :: [any()].
filter_options([], _, Acc) ->
  Acc;
filter_options([Opt = {Key, _}|Tail], AllowedKeys, Acc) ->
  case lists:member(Key, AllowedKeys) of
    true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
    false -> filter_options(Tail, AllowedKeys, Acc)
  end;
filter_options([Opt = {raw, _, _, _}|Tail], AllowedKeys, Acc) ->
  case lists:member(raw, AllowedKeys) of
    true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
    false -> filter_options(Tail, AllowedKeys, Acc)
  end;
filter_options([Opt|Tail], AllowedKeys, Acc) when is_atom(Opt) ->
  case lists:member(Opt, AllowedKeys) of
    true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
    false -> filter_options(Tail, AllowedKeys, Acc)
  end.


recv(Socket, Length) ->
  recv(Socket, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_udp:recv/3
-spec recv(socks5_socket(), non_neg_integer(), timeout())
    -> {ok, any()} | {error, closed | atom()}.
recv({_, Socket, _, _}, Length, Timeout) ->
  case gen_udp:recv(Socket, Length, Timeout) of
    {ok, Data} -> {ok, remove_header(Data)};
    Error -> Error
  end.


%% @doc Send a packet on a socket.
%% @see gen_udp:send/2
-spec send(socks5_socket(), any(), integer(), iolist()) -> ok | {error, atom()}.
send({_, Socket, ProxyHost, ProxyPort}, Host, Port, Packet) ->
  Addr = addr(Host, Port),
  Packet1 = [<< 0, 0, 0, Addr/binary>>] ++ Packet,
  gen_udp:send(Socket, ProxyHost, ProxyPort, Packet1).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(socks5_socket(), list()) -> ok | {error, atom()}.
setopts({_, Socket, _, _}, Opts) ->
  inet:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_udp:controlling_process/2
-spec controlling_process(socks5_socket(), pid())
    -> ok | {error, closed | not_owner | atom()}.
controlling_process({_, Socket, _, _}, Pid) ->
  gen_udp:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(socks5_socket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername({_, Socket, _, _}) ->
  inet:peername(Socket).

%% @doc Close a socks5 socket.
%% @see gen_tcp:close/1
%% @see gen_udp:close/1
-spec close(socks5_socket()) -> ok.
close({TcpSocket, UdpSocket, _, _}) ->
  gen_udp:close(UdpSocket),
  gen_tcp:close(TcpSocket).

%% @doc Immediately close a socket in one or two directions.
%% @see gen_tcp:shutdown/2
-spec shutdown(socks5_socket(), read | write | read_write) -> ok.
shutdown({Socket, _, _, _}, How) ->
  gen_tcp:shutdown(Socket, How).

%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(socks5_socket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname({_, Socket, _, _}) ->
  inet:sockname(Socket).

%%                   +----+----------+----------+
%%                   |VER | NMETHODS | METHODS  |
%%                   +----+----------+----------+
%%                   | 1  |    1     | 1 to 255 |
%%                   +----+----------+----------+
%%
%%   The VER field is set to X'05' for this version of the protocol.  The
%%   NMETHODS field contains the number of method identifier octets that
%%   appear in the METHODS field.
%%
%%   The server selects from one of the methods given in METHODS, and
%%   sends a METHOD selection message:
%%
%%                         +----+--------+
%%                         |VER | METHOD |
%%                         +----+--------+
%%                         | 1  |   1    |
%%                         +----+--------+
%%
%%   If the selected METHOD is X'FF', none of the methods listed by the
%%   client are acceptable, and the client MUST close the connection.
%%
%%   The values currently defined for METHOD are:
%%
%%          o  X'00' NO AUTHENTICATION REQUIRED
%%          o  X'01' GSSAPI
%%          o  X'02' USERNAME/PASSWORD
%%          o  X'03' to X'7F' IANA ASSIGNED
%%          o  X'80' to X'FE' RESERVED FOR PRIVATE METHODS
%%          o  X'FF' NO ACCEPTABLE METHODS
do_handshake(Socket, Host, Port, Options) ->
  ProxyUser = proplists:get_value(socks5_user, Options),
  ProxyPass = proplists:get_value(socks5_pass, Options, <<>>),
  case ProxyUser of
    undefined ->
      %% no auth
      ok = gen_tcp:send(Socket, << 5, 1, 0 >>),
      case gen_tcp:recv(Socket, 2, ?TIMEOUT) of
        {ok, << 5, 0 >>} ->
          do_connection(Socket, Host, Port);
        {ok, _Reply} ->
          {error, unknown_reply};
        Error ->
          Error
      end;
    _ ->
      case do_authentication(Socket, ProxyUser, ProxyPass) of
        ok ->
          do_connection(Socket, Host, Port);
        Error ->
          Error
      end
  end.

do_authentication(Socket, User, Pass) ->
  ok = gen_tcp:send(Socket, << 5, 1, 2 >>),
  case gen_tcp:recv(Socket, 2, ?TIMEOUT) of
    {ok, <<5, 0>>} ->
      ok;
    {ok, <<5, 2>>} ->
      UserLength = byte_size(User),
      PassLength = byte_size(Pass),
      Msg = iolist_to_binary([<< 1, UserLength >>,
        User, << PassLength >>,
        Pass]),
      ok = gen_tcp:send(Socket, Msg),
      case gen_tcp:recv(Socket, 2, ?TIMEOUT) of
        {ok, <<1, 0>>} ->
          ok;
        _ ->
          {error, not_authenticated}
      end;
    _ ->
      {error, not_authenticated}
  end.

%%   The SOCKS request is formed as follows:
%%
%%        +----+-----+-------+------+----------+----------+
%%        |VER | CMD |  RSV  | ATYP | DST.ADDR | DST.PORT |
%%        +----+-----+-------+------+----------+----------+
%%        | 1  |  1  | X'00' |  1   | Variable |    2     |
%%        +----+-----+-------+------+----------+----------+
%%
%%     Where:
%%
%%          o  VER    protocol version: X'05'
%%          o  CMD
%%             o  CONNECT X'01'
%%             o  BIND X'02'
%%             o  UDP ASSOCIATE X'03'
%%          o  RSV    RESERVED
%%          o  ATYP   address type of following address
%%             o  IP V4 address: X'01'
%%             o  DOMAINNAME: X'03'
%%             o  IP V6 address: X'04'
%%          o  DST.ADDR       desired destination address
%%          o  DST.PORT desired destination port in network octet
%%             order
do_connection(Socket, Host, Port) ->
  case addr(Host, Port) of
    Addr when is_binary(Addr) ->
      ok = gen_tcp:send(Socket, << 5, 3, 0, Addr/binary >>),
      case gen_tcp:recv(Socket, 4, ?TIMEOUT) of
        {ok, << 5, 0, 0, AType>>} ->
          BoundAddr = recv_addr_port(AType, gen_tcp, Socket),
          check_connection(BoundAddr);
        {ok, _} ->
          {error, badarg};
        Error ->
          Error
      end;
    Error ->
      Error
  end.

addr(Host, Port) when is_list(Host) ->
  case inet_parse:address(Host) of
    {ok, {IP1, IP2, IP3, IP4}} ->
      << 1, IP1, IP2, IP3, IP4, Port:16 >>;
    {ok, {IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}} ->
      << 4, IP1:16, IP2:16, IP3:16, IP4:16, IP5:16, IP6:16, IP7:16, IP8:16, Port:16 >>;
    _ -> %% domain name
      Host1 = list_to_binary(Host),
      HostLength = byte_size(Host1),
      << 3, HostLength, Host1/binary, Port:16 >>
  end;

addr({IP1, IP2, IP3, IP4}, Port) ->
  << 1, IP1, IP2, IP3, IP4, Port:16 >>;

addr({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}, Port) ->
  << 4, IP1:16, IP2:16, IP3:16, IP4:16, IP5:16, IP6:16, IP7:16, IP8:16, Port:16 >>.

recv_addr_port(1 = AType, Transport, Socket) -> % IPv4
   {ok, Data} = Transport:recv(Socket, 6, ?TIMEOUT),
   <<AType, Data/binary>>;
recv_addr_port(4 = AType, Transport, Socket) -> % IPv6
   {ok, Data} = Transport:recv(Socket, 18, ?TIMEOUT),
   <<AType, Data/binary>>;
recv_addr_port(3 = AType, Transport, Socket) -> % Domain
   {ok, <<DLen/integer>>} = Transport:recv(Socket, 1, ?TIMEOUT),
   {ok, AddrPort} = Transport:recv(Socket, DLen + 2, ?TIMEOUT),
   <<AType, DLen, AddrPort/binary>>;
recv_addr_port(_, _, _) ->
   error.

remove_header(<< 0, 0, 0, 1, _Ip:32, _Port:16, Data/binary >>) -> % IPv4
   Data;
remove_header(<< 0, 0, 0, 4, _Ip:128, _Port:16, Data/binary >>) -> % IPv6
   Data;
remove_header(<< 0, 0, 0, 3, Len:8/integer, _Domain:Len/binary, _Port:16, Data/binary >>) -> % Domain
   Data;
remove_header(Data) ->
   Data.

check_connection(<< 3, DomainLen:8, Domain:DomainLen, Port:16 >>) ->
  {ok, Domain, Port};
check_connection(<< 1, IP1:8, IP2:8, IP3:8, IP4:8, Port:16 >>) ->
  {ok, {IP1, IP2, IP3, IP4}, Port};
check_connection(<< 4, IP1:16, IP2:16, IP3:16, IP4:16, IP5:16, IP6:16, IP7:16, IP8:16, Port:16 >>) ->
  {ok, {IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}, Port};
check_connection(_) ->
  {error, no_connection}.
