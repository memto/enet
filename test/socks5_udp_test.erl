-module(socks5_udp_test).

-include_lib("enet/src/enet_commands.hrl").

-export([
         udp_test/0
        ]).

%% c("test/socks5_udp_test.erl").
%% socks5_udp_test:udp_test().
udp_test() ->
  ProxyHost = "0.0.0.0",
  ProxyPort = 9969,
  Opts = [{socks5_host, ProxyHost}, {socks5_port, ProxyPort}],
  Host = "0.0.0.0",
  Port = 17000,
  Self = self(),
  ConnectFun = fun(PeerInfo) ->
                   Self ! PeerInfo,
                   {ok, Self}
               end,
  Compressor = enet_compress:enet_get_compressor(range_coder),
  {ok, Client}  = enet:start_host(0, ConnectFun, Compressor, [{peer_limit, 1}]),
  {ok, Peer} = enet:connect_peer(Client, Host, Port, 2, Opts),
  io:fwrite("Client= ~w Peer= ~w ~n", [Client, Peer]),

  loop(50000, Client),

  ok = enet:disconnect_peer(Peer),
  ok = enet:stop_host(Client).
%%  Value.


loop(Timeout, Client) ->
  receive
    #{peer := Peer, channels := Channels, connect_id := ConnectID, remote_ip := RIp, remote_port := RPort} = Remote ->
      io:fwrite("connected remote= ~w ~n", [Remote]),
      {ok, LocalChannel1}  = maps:find(0, Channels),
      ok = enet:send_reliable(LocalChannel1, <<"local->remote 1", 0>>),
      loop(Timeout, Client);
    {enet, 0, #reliable{ data = Data }} ->
      io:fwrite("recv something: ~w ~n", [Data]),
      loop(Timeout, Client);
    Evt ->
      io:fwrite("receive Evt: ~w ~n", [Evt]),
      loop(Timeout, Client)
  % after Timeout ->
  %     ok = enet:stop_host(Client),
  %     exit(remote_channel_did_not_send_data_to_worker)
  end.