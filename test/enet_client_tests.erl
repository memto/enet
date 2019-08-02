-module(enet_client_tests).

-include_lib("eunit/include/eunit.hrl").

% -include_lib("enet/include/enet.hrl").
-include_lib("enet/src/enet_commands.hrl").

-export([
         create_enet_client/0,
         check_crc/6
        ]).

%    c("test/enet_client_tests.erl"). enet_client_tests:create_enet_client().
create_enet_client() ->
  Self = self(),

  ConnectFun = fun(PeerInfo) ->
                   Self ! PeerInfo,
                   {ok, Self}
               end,

  Compressor = enet_compress:enet_get_compressor(range_coder),
  % Compressor = enet_compress2:enet_get_compressor(range_coder),

  {ok, Client}  = enet:start_host(0, ConnectFun, Compressor, [{peer_limit, 1}]),
  {ok, Peer} = enet:connect_peer(Client, {127,0,0,1}, 18000, 2),
  % {ok, Peer} = enet:connect_peer(Client, {127,0,0,1}, 17094, 2),
  % {ok, Peer} = enet:connect_peer(Client, {127,0,0,1}, 17000, 2),
  % {ok, Peer} = enet:connect_peer(Client, {45,77,23,123}, 18000, 2),
  % {ok, Peer} = enet:connect_peer(Client, {209,59,190,105}, 17096, 2),

  % 209.59.190.105:17096

  io:fwrite("Client= ~w Peer= ~w ~n", [Client, Peer]),

  loop(50000, Client),

  ok = enet:disconnect_peer(Peer),
  ok = enet:stop_host(Client)
  .

loop(Timeout, Client) ->
  receive
    #{peer := Peer, channels := Channels, connect_id := ConnectID, remote_ip := RIp, remote_port := RPort} = Remote ->
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


check_crc2(ConnectID, Payload) ->
  ok
  .

% [{{:command_header, 0, 0, 1, 255, 1}, {:acknowledge, 1, 14369}}, {{:command_header, 1, 0, 5, 255, 2}, {:ping}}]
check_crc(PeerID, ChannelID, Seq, SentTime, AckSentTime, ConnectID) ->
  % -record(protocol_header,
  %         {
  %           compressed = 0,
  %           session_id = 0,
  %           peer_id    = ?MAX_PEER_ID,
  %           sent_time  = undefined
  %         }).

  PH = #protocol_header{
            peer_id = PeerID,
            sent_time = SentTime
          },

  H = #command_header{
       channel_id = ChannelID,
       reliable_sequence_number = Seq
      },

  {AckH, AckC} = enet_command:acknowledge(H, AckSentTime),

  HBin = enet_protocol_encode:command_header(AckH),
  CBin = enet_protocol_encode:command(AckC),

  {H2, C2} = enet_command:ping(Seq+1),

  HBin2 = enet_protocol_encode:command_header(H2),
  CBin2 = enet_protocol_encode:command(C2),
  Commands = [HBin, CBin, HBin2, CBin2],

  PHBin = enet_protocol_encode:protocol_header(PH),
  CommandsBin = case is_list(Commands) of
    true ->
      binary:list_to_bin(Commands);
    false ->
      Commands
  end,

  io:fwrite("~w ~n", [{AckH, AckC, H2, C2}]),
  io:fwrite("~w ~n", [PHBin]),
  io:fwrite("~w ~n", [<<HBin/binary, CBin/binary>>]),
  io:fwrite("~w ~n", [<<HBin2/binary, CBin2/binary>>]),

  Payload =
  if
    (PeerID >= 16#FFF) or (ConnectID == undefined) ->
      io:fwrite("Checksum No ConnectID ~n"),
      <<PHBin/binary, 0:32, CommandsBin/binary>>;
    true ->
      % <<A,B,C,D>> = binary:encode_unsigned(ConnectID),
      % ConnectIDBin = <<D, C, B, A>>,
      % <<ConnectID2:32>> = ConnectIDBin,
      % io:fwrite("Checksum ConnectID ~w/~w ~n", [ConnectID, ConnectID2]),
      % <<PHBin/binary, ConnectIDBin/binary, CommandsBin/binary>>

      io:fwrite("Checksum ConnectID ~w ~n", [ConnectID]),
      <<PHBin/binary, ConnectID:32, CommandsBin/binary>>
  end,
  io:fwrite("checksum input: ~w ~n ", [Payload]),
  erlang:crc32(Payload)
  .