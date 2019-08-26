-module(enet_command).

-include("enet_commands.hrl").
-include("enet_protocol.hrl").

-export([
         acknowledge/2,
         connect/12,
         verify_connect/13,
         sequenced_disconnect/0,
         unsequenced_disconnect/0,
         ping/1,
         send_unsequenced/2,
         send_unreliable/4,
         send_reliable/3,
         send_fragment/9,
         command_name/1
        ]).

-export([
         clamp/3,
         calculate_window_size/2,
         calculate_session_id/2
        ]).

command_name(CmdNum) ->
  CommandNames = #{
      1 => "COMMAND_ACKNOWLEDGE"             ,
      2 => "COMMAND_CONNECT"                 ,
      3 => "COMMAND_VERIFY_CONNECT"          ,
      4 => "COMMAND_DISCONNECT"              ,
      5 => "COMMAND_PING"                    ,
      6 => "COMMAND_SEND_RELIABLE"           ,
      7 => "COMMAND_SEND_UNRELIABLE"         ,
      8 => "COMMAND_SEND_FRAGMENT"           ,
      9 => "COMMAND_SEND_UNSEQUENCED"        ,
     10 => "COMMAND_BANDWIDTH_LIMIT"         ,
     11 => "COMMAND_THROTTLE_CONFIGURE"      ,
     12 => "COMMAND_SEND_UNRELIABLE_FRAGMENT"
  },
  maps:get(CmdNum, CommandNames).

acknowledge(H = #command_header{}, SentTime) ->
    #command_header{
       channel_id = ChannelID,
       reliable_sequence_number = N
      } = H,
    {
      #command_header{
         command = ?COMMAND_ACKNOWLEDGE,
         channel_id = ChannelID,
         reliable_sequence_number = N
        },
      #acknowledge{
         received_reliable_sequence_number = N,
         received_sent_time = SentTime
        }
    }.


connect(OutgoingPeerID,
        IncomingSessionID,
        OutgoingSessionID,
        ChannelCount,
        MTU,
        IncomingBandwidth,
        OutgoingBandwidth,
        PacketThrottleInterval,
        PacketThrottleAcceleration,
        PacketThrottleDeceleration,
        ConnectID,
        OutgoingReliableSequenceNumber) ->
    WindowSize = calculate_initial_window_size(OutgoingBandwidth),
    {
      #command_header{
         command = ?COMMAND_CONNECT,
         please_acknowledge = 1,
         reliable_sequence_number = OutgoingReliableSequenceNumber
        },
      #connect{
         outgoing_peer_id = OutgoingPeerID,
         incoming_session_id = IncomingSessionID,
         outgoing_session_id = OutgoingSessionID,
         mtu = MTU,
         window_size = WindowSize,
         channel_count = ChannelCount,
         incoming_bandwidth = IncomingBandwidth,
         outgoing_bandwidth = OutgoingBandwidth,
         packet_throttle_interval = PacketThrottleInterval,
         packet_throttle_acceleration = PacketThrottleAcceleration,
         packet_throttle_deceleration = PacketThrottleDeceleration,
         connect_id = ConnectID,
         data = 0 %% What is this used for?
        }
    }.


verify_connect(OutgoingPeerID,
               IncomingSessionID,
               OutgoingSessionID,
               MTU,
               WindowSize,
               ChannelCount,
               IncomingBandwidth,
               OutgoingBandwidth,
               PacketThrottleInterval,
               PacketThrottleAcceleration,
               PacketThrottleDeceleration,
               ConnectID,
               OutgoingReliableSequenceNumber) ->
    {
      #command_header{
         command = ?COMMAND_VERIFY_CONNECT,
         please_acknowledge = 1,
         reliable_sequence_number = OutgoingReliableSequenceNumber
        },
      #verify_connect{
         outgoing_peer_id = OutgoingPeerID,
         incoming_session_id = IncomingSessionID,
         outgoing_session_id = OutgoingSessionID,
         mtu = MTU,
         window_size = WindowSize,
         channel_count = ChannelCount,
         incoming_bandwidth = IncomingBandwidth,
         outgoing_bandwidth = OutgoingBandwidth,
         packet_throttle_interval = PacketThrottleInterval,
         packet_throttle_acceleration = PacketThrottleAcceleration,
         packet_throttle_deceleration = PacketThrottleDeceleration,
         connect_id = ConnectID
        }
    }.


sequenced_disconnect() ->
    {
      #command_header{
         please_acknowledge = 1,
         command = ?COMMAND_DISCONNECT
        },
      #disconnect{}
    }.


unsequenced_disconnect() ->
    {
      #command_header{
         unsequenced = 1,
         command = ?COMMAND_DISCONNECT
        },
      #disconnect{}
    }.


ping(Seq) ->
    {
      #command_header{
         please_acknowledge = 1,
         command = ?COMMAND_PING,
         reliable_sequence_number = Seq
        },
      #ping{}
    }.


send_unsequenced(ChannelID, Data) ->
    {
      #command_header{
         unsequenced = 1,
         command = ?COMMAND_SEND_UNSEQUENCED,
         channel_id = ChannelID
        },
      #unsequenced{
         data = Data
        }
    }.

send_unreliable(ChannelID, ReliableSequenceNumber, SequenceNumber, Data) ->
    {
      #command_header{
         command = ?COMMAND_SEND_UNRELIABLE,
         channel_id = ChannelID,
         reliable_sequence_number = ReliableSequenceNumber
        },
      #unreliable{
         sequence_number = SequenceNumber,
         data = Data
        }
    }.

send_reliable(ChannelID, ReliableSequenceNumber, Data) ->
    {
      #command_header{
         please_acknowledge = 1,
         command = ?COMMAND_SEND_RELIABLE,
         channel_id = ChannelID,
         reliable_sequence_number = ReliableSequenceNumber
        },
      #reliable{
         data = Data
        }
    }.

send_fragment(ChannelID, ReliableSequenceNumber, StartSN, DataLen, FragCnt, FragNum, TotalLen, FragOff, Data) ->
    {
      #command_header{
         please_acknowledge = 1,
         command = ?COMMAND_SEND_FRAGMENT,
         channel_id = ChannelID,
         reliable_sequence_number = ReliableSequenceNumber
        },
      #fragment{
          start_sequence_number = StartSN,
          data_length           = DataLen,
          fragment_count        = FragCnt,
          fragment_number       = FragNum,
          total_length          = TotalLen,
          fragment_offset       = FragOff,
          data                  = Data
        }
    }.

clamp(X, Max, Min) ->
    max(Min, min(Max, X)).

select_smallest(A, B, Max, Min) ->
    clamp(min(A, B), Max, Min).


calculate_window_size(0, ConnectWindowSize) ->
    clamp(ConnectWindowSize, ?MAX_WINDOW_SIZE, ?MIN_WINDOW_SIZE);
calculate_window_size(IncomingBandwidth, ConnectWindowSize) ->
    InitialWindowSize =
        ?MIN_WINDOW_SIZE * IncomingBandwidth / ?PEER_WINDOW_SIZE_SCALE,
    select_smallest(InitialWindowSize,
                    ConnectWindowSize,
                    ?MAX_WINDOW_SIZE,
                    ?MIN_WINDOW_SIZE).

calculate_initial_window_size(0) ->
    ?MAX_WINDOW_SIZE;
calculate_initial_window_size(OutgoingBandwidth) ->
    InitialWindowSize =
        ?MAX_WINDOW_SIZE * OutgoingBandwidth / ?PEER_WINDOW_SIZE_SCALE,
    clamp(InitialWindowSize, ?MAX_WINDOW_SIZE, ?MIN_WINDOW_SIZE).



calculate_session_id(ConnectSessionID, PeerSessionID) ->
    InitialSessionID =
        case ConnectSessionID of
            16#FF -> PeerSessionID;
            _     -> ConnectSessionID
        end,
    case (InitialSessionID + 1) band 2#11 of
        PeerSessionID     -> (PeerSessionID + 1) band 2#11;
        IncomingSessionID -> IncomingSessionID
    end.
