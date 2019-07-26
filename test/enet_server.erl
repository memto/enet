-module(enet_server).

-include_lib("enet/include/enet.hrl").

-export([
         create_enet_server/0
        ]).

create_enet_server() ->
  ListeningPort = 7777,
  Self = self(),
  ConnectFun = fun(PeerInfo) ->
                   Self ! PeerInfo,
                   {ok, Self}
               end,

  Compressor = enet_compress:enet_get_compressor(range_coder),
  % io:fwrite("Compressor ~w ~n", [Compressor]),

  {ok, Server}  = enet:start_host(ListeningPort, ConnectFun, Compressor, [{peer_limit, 8}]),

  receive
      {enet, 0, #reliable{ data = Data1 }} ->
      io:fwrite("receive: [~s] ~n", [Data1]),
      ok
  after 50000 ->
          ok = enet:stop_host(Server),
          exit(remote_channel_did_not_send_data_to_worker)
  end,

  receive
      {enet, 0, #reliable{ data = Data2 }} ->
      io:fwrite("receive: [~s] ~n", [Data2]),
      ok
  after 50000 ->
          ok = enet:stop_host(Server),
          exit(remote_channel_did_not_send_data_to_worker)
  end,

  receive
      {enet, 0, #reliable{ data = Data3 }} ->
      io:fwrite("receive: [~s] ~n", [Data3]),
      ok
  after 50000 ->
          ok = enet:stop_host(Server),
          exit(remote_channel_did_not_send_data_to_worker)
  end,

  ok = enet:stop_host(Server).


% ListeningPort = 1234,
% ConnectFun = fun(PeerInfo) ->
%                      server_supervisor:start_worker(PeerInfo)
%              end,
% Options = [{peer_limit, 8}, {channel_limit, 3}],
% {ok, Host} = enet:start_host(ListeningPort, ConnectFun, Options),
% ...
% ...
% ...
% enet:stop_host(Host).