-module(enet_server).

-include_lib("enet/include/enet.hrl").

-export([
         create_enet_server/0
        ]).

create_enet_server() ->
  ListeningPort = 17000,
  Self = self(),
  ConnectFun = fun(PeerInfo) ->
                   Self ! PeerInfo,
                   {ok, Self}
               end,

  Compressor = enet_compress:enet_get_compressor(range_coder),
  % io:fwrite("Compressor ~w ~n", [Compressor]),

  {ok, Server}  = enet:start_host(ListeningPort, ConnectFun, Compressor, [{peer_limit, 8}]),

  % {ConnectID, LocalChannels} =
  %   receive
  %       #{peer := LocalPeer, channels := LCs, connect_id := C} -> {C, LCs}
  %   after 50000 ->
  %         ok = enet:stop_host(Server),
  %         exit(local_peer_did_not_notify_worker)
  %   end,

  % io:fwrite("ConnectID: ~w, LocalChannels: ~w ~n", [ConnectID, LocalChannels]),

  % {ok, LocalChannel1}  = maps:find(0, LocalChannels),
  % ok = enet:send_reliable(LocalChannel1, <<"I AM SERVER">>),

  loop(50000, Server),

  % receive
  %     {enet, 0, #reliable{ data = Data1 }} ->
  %     io:fwrite("receive: [~s] ~n", [Data1]),
  %     ok
  % after 50000 ->
  %         ok = enet:stop_host(Server),
  %         exit(remote_channel_did_not_send_data_to_worker)
  % end,

  % receive
  %     {enet, 0, #reliable{ data = Data2 }} ->
  %     io:fwrite("receive: [~s] ~n", [Data2]),
  %     ok
  % after 50000 ->
  %         ok = enet:stop_host(Server),
  %         exit(remote_channel_did_not_send_data_to_worker)
  % end,

  % receive
  %     {enet, 0, #reliable{ data = Data3 }} ->
  %     io:fwrite("receive: [~s] ~n", [Data3]),
  %     ok
  % after 50000 ->
  %         ok = enet:stop_host(Server),
  %         exit(remote_channel_did_not_send_data_to_worker)
  % end,

  ok = enet:stop_host(Server).


loop(Timeout, Server) ->
  receive
      Evt ->
      io:fwrite("receive: [~w] ~n", [Evt]),
      loop(Timeout, Server)
  after Timeout ->
      ok = enet:stop_host(Server),
      exit(remote_channel_did_not_send_data_to_worker)
  end.


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