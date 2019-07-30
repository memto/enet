% c("test/enet_client.erl"). enet_client:create_enet_client().


-module(enet_client).

-include_lib("enet/include/enet.hrl").

-export([
         create_enet_client/0
        ]).

create_enet_client() ->
  Self = self(),

  ConnectFun = fun(PeerInfo) ->
                   Self ! PeerInfo,
                   {ok, Self}
               end,

  Compressor = enet_compress:enet_get_compressor(range_coder),

  {ok, Client}  = enet:start_host(0, ConnectFun, Compressor, [{peer_limit, 1}]),
  {ok, Peer} = enet:connect_peer(Client, {127,0,0,1}, 17094, 2),

  io:fwrite("Client= ~w Peer= ~w ~n", [Client, Peer]),

  loop(50000, Client),

  ok = enet:stop_host(Client)
  .


loop(Timeout, Client) ->
  receive
    #{channels := RChanels ,connect_id := ConnectID, ip := RIp, peer := RPeer, port := RPort} = Remote ->
      io:fwrite("connected remote= ~w ~n", [Remote]),
      loop(Timeout, Client);
    {enet, 0, #reliable{ data = Data }} ->
      io:fwrite("recv something: ~w. bye!! ~n~n", [Data]);
    Evt ->
      io:fwrite("receive Evt: ~w ~n", [Evt]),
      loop(Timeout, Client)
  % after Timeout ->
  %     ok = enet:stop_host(Client),
  %     exit(remote_channel_did_not_send_data_to_worker)
  end.
