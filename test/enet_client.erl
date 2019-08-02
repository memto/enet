-module(enet_client).

% -include_lib("enet/include/enet.hrl").
-include_lib("enet/src/enet_commands.hrl").

-export([
         create_enet_client/0
        ]).


% c("test/enet_client.erl"). enet_client:create_enet_client().
create_enet_client() ->
  Self = self(),

  ConnectFun = fun(PeerInfo) ->
                   Self ! PeerInfo,
                   {ok, Self}
               end,

  Compressor = enet_compress:enet_get_compressor(range_coder),
  % Compressor = enet_compress2:enet_get_compressor(range_coder),

  {ok, Client}  = enet:start_host(0, ConnectFun, Compressor, [{peer_limit, 1}]),
  % {ok, Peer} = enet:connect_peer(Client, {209,59,190,105}, 17127, 2),
  {ok, Peer} = enet:connect_peer(Client, {127,0,0,1}, 18000, 2),
  % {ok, Peer} = enet:connect_peer(Client, {127,0,0,1}, 17094, 2),
  % {ok, Peer} = enet:connect_peer(Client, {45,77,23,123}, 18000, 2),

  io:fwrite("Client= ~w Peer= ~w ~n", [Client, Peer]),

  loop(50000, Client),

  ok = enet:disconnect_peer(Peer),
  ok = enet:stop_host(Client)
  .


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
