-module(enet_server).

-include_lib("enet/include/enet.hrl").

-export([
         create_enet_server/0
        ]).

%    c("test/enet_server.erl"). enet_server:create_enet_server().
create_enet_server() ->
  ListeningPort = 17000,
  Self = self(),
  ConnectFun = fun(PeerInfo) ->
                   Self ! PeerInfo,
                   {ok, Self}
               end,

  Compressor = enet_compress:enet_get_compressor(range_coder),
  % io:fwrite("Compressor ~w ~n", [Compressor]),

  {ok, Server}  = enet:start_host(ListeningPort, ConnectFun, Compressor, [{peer_limit, 10}]),

  loop(50000, Server),

  ok = enet:stop_host(Server).


loop(Timeout, Server) ->
  receive
      #{channels := Channels, connect_id := ConnectID, ip := IP, peer := Peer, port := Port, session_id := SessionID} = Evt ->
        io:fwrite("A new client connected ~w ~n", [Evt]),
        {ok, LocalChannel1}  = maps:find(0, Channels),
        ok = enet:send_reliable(LocalChannel1, <<"local->remote 1">>),
        loop(Timeout, Server);
      Evt ->
        io:fwrite("receive: [~w] ~n", [Evt]),
        loop(Timeout, Server)
  % after Timeout ->
  %     ok = enet:stop_host(Server),
  %     exit(remote_channel_did_not_send_data_to_worker)
  end.