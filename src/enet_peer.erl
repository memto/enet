-module(enet_peer).
-behaviour(gen_statem).

-include("enet_peer.hrl").
-include("enet_commands.hrl").
-include("enet_protocol.hrl").
-include("enet_helpers.hrl").

%% API
-export([
         start_link/2,
         disconnect/1,
         disconnect_now/1,
         channels/1,
         channel/2,
         recv_incoming_packet/4,
         send_command/2,
         send_reliable_outgoing_commands/2,
         get_connect_id/1,
         get_mtu/1,
         get_name/1,
         get_local_peer_id/1,
         get_pool/1,
         get_pool_worker_id/1
        ]).

%% gen_statem callbacks
-export([
         init/1,
         callback_mode/0,
         terminate/3,
         code_change/4
        ]).

%% gen_statem state functions
-export([
         connecting/3,
         acknowledging_connect/3,
         acknowledging_verify_connect/3,
         verifying_connect/3,
         connected/3,
         disconnecting/3
        ]).

-record(state,
        {
         local_port,
         remote_ip,
         remote_port,
         local_peer_id,
         remote_peer_id = ?MAX_PEER_ID,
         incoming_session_id = 16#FF,
         outgoing_session_id = 16#FF,
         incoming_bandwidth = 0,
         outgoing_bandwidth = 0,
         window_size = ?MAX_WINDOW_SIZE,
         mtu = ?MAX_MTU,
         packet_throttle_interval = ?PEER_PACKET_THROTTLE_INTERVAL,
         packet_throttle_acceleration = ?PEER_PACKET_THROTTLE_ACCELERATION,
         packet_throttle_deceleration = ?PEER_PACKET_THROTTLE_DECELERATION,
         roundTripTime = ?PEER_DEFAULT_ROUND_TRIP_TIME,
         roundTripTimeVariance = 0,
         outgoing_reliable_sequence_number = 1,
         incoming_unsequenced_group = 0,
         outgoing_unsequenced_group = 1,
         unsequenced_window = 0,
         connect_id,
         host,
         channel_count,
         channels,
         worker,
         connect_fun
        }).


%%==============================================================
%% Connection handshake
%%==============================================================
%%
%%
%%      state    client              server     state
%%          (init) *
%%                 |       connect
%%                 |------------------->* (init)
%%    'connecting' |                    | 'acknowledging connect'
%%                 |     ack connect    |
%%                 |<-------------------|
%%  'acknowledging |                    |
%% verify connect' |                    |
%%                 |   verify connect   |
%%                 |<-------------------|
%%                 |                    | 'verifying connect'
%%                 | ack verify connect |
%%                 |------------------->|
%%     'connected' |                    | 'connected'
%%                 |                    |
%%
%%
%%==============================================================

%%==============================================================
%% Disconnect procedure
%%==============================================================
%%
%%
%%      state   client               server   state
%%               peer                 peer
%%                 |                    |
%%     'connected' |                    | 'connected'
%%                 |     disconnect     |
%%                 |------------------->|
%% 'disconnecting' |                    |
%%                 |   ack disconnect   |
%%                 |<-------------------|
%%          (exit) |                    | (exit)
%%                 *                    *
%%
%%
%%==============================================================


%%%===================================================================
%%% API
%%%===================================================================

start_link(LocalPort, Peer) ->
    gen_statem:start_link(?MODULE, [LocalPort, Peer], []).

disconnect(Peer) ->
    gen_statem:cast(Peer, disconnect).

disconnect_now(Peer) ->
    gen_statem:cast(Peer, disconnect_now).

channels(Peer) ->
    gen_statem:call(Peer, channels).

channel(Peer, ID) ->
    gen_statem:call(Peer, {channel, ID}).

recv_incoming_packet(Peer, FromIP, SentTime, Packet) ->
    gen_statem:cast(Peer, {incoming_packet, FromIP, SentTime, Packet}).

send_reliable_outgoing_commands(Peer, Commands) ->
    gen_statem:cast(Peer, {send_reliable_outgoing_commands, Commands}).

send_command(Peer, {H, C}) ->
    gen_statem:cast(Peer, {outgoing_command, {H, C}}).

get_connect_id(Peer) ->
    gproc:get_value({p, l, connect_id}, Peer).

get_mtu(Peer) ->
    gproc:get_value({p, l, mtu}, Peer).

get_name(Peer) ->
    gproc:get_value({p, l, name}, Peer).

get_local_peer_id(Peer) ->
    gproc:get_value({p, l, local_peer_id}, Peer).

get_pool(Peer) ->
    gen_statem:call(Peer, pool).

get_pool_worker_id(Peer) ->
    gen_statem:call(Peer, pool_worker_id).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([LocalPort, P = #enet_peer{ handshake_flow = local }]) ->
    %%
    %% The client application wants to connect to a remote peer.
    %%
    %% - Send a Connect command to the remote peer (use peer ID)
    %% - Start in the 'connecting' state
    %%
    #enet_peer{
       local_peer_id = PeerID,
       remote_ip = IP,
       remote_port = Port,
       name = Ref,
       host = Host,
       channels = N,
       connect_fun = ConnectFun
      } = P,
    enet_pool:connect_peer(LocalPort, Ref),
    gproc:reg({n, l, {enet_peer, Ref}}),
    gproc:reg({p, l, name}, Ref),
    gproc:reg({p, l, local_peer_id}, PeerID),
    S = #state{
           host = Host,
           local_port = LocalPort,
           remote_ip = IP,
           remote_port = Port,
           local_peer_id = PeerID,
           channel_count = N,
           connect_fun = ConnectFun
          },
    {ok, connecting, S};

init([LocalPort, P = #enet_peer{ handshake_flow = remote }]) ->
    %%
    %% A remote peer wants to connect to the client application.
    %%
    %% - Start in the 'acknowledging_connect' state
    %% - Handle the received Connect command
    %%
    #enet_peer{
       local_peer_id = PeerID,
       remote_ip = IP,
       remote_port = Port,
       name = Ref,
       host = Host,
       connect_fun = ConnectFun
      } = P,

    enet_pool:connect_peer(LocalPort, Ref),
    gproc:reg({n, l, {enet_peer, Ref}}),
    gproc:reg({p, l, name}, Ref),
    gproc:reg({p, l, local_peer_id}, PeerID),
    S = #state{
           host = Host,
           local_port = LocalPort,
           remote_ip = IP,
           remote_port = Port,
           local_peer_id = PeerID,
           connect_fun = ConnectFun
          },
    {ok, acknowledging_connect, S}.


callback_mode() ->
    [state_functions, state_enter].


%%%
%%% Connecting state
%%%

connecting(enter, _OldState, S) ->
    %%
    %% Sending the initial Connect command.
    %%
    #state{
       host = Host,
       channel_count = ChannelCount,
       remote_ip = IP,
       remote_port = Port,
       local_peer_id = PeerID,
       incoming_session_id = IncomingSessionID,
       outgoing_session_id = OutgoingSessionID,
       packet_throttle_interval = PacketThrottleInterval,
       packet_throttle_acceleration = PacketThrottleAcceleration,
       packet_throttle_deceleration = PacketThrottleDeceleration,
       outgoing_reliable_sequence_number = SequenceNr
      } = S,

    IncomingBandwidth = enet_host:get_incoming_bandwidth(Host),
    OutgoingBandwidth = enet_host:get_outgoing_bandwidth(Host),
    MTU = enet_host:get_mtu(Host),
    <<ConnectID:32>> = crypto:strong_rand_bytes(4),
    {ConnectH, ConnectC} =
        enet_command:connect(
          PeerID,
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
          SequenceNr),

    ?DBG_PEER_HS("<<|| [~w] connecting ~s ~w ~n", [S#state.local_port, enet_command:command_name(ConnectH), {ConnectH, ConnectC}]),

    HBin = enet_protocol_encode:command_header(ConnectH),
    CBin = enet_protocol_encode:command(ConnectC),
    Data = [HBin, CBin],
    {sent_time, _SentTime} =
        enet_host:send_outgoing_commands(Host, Data, ConnectID, OutgoingSessionID, IP, Port),

    ChannelID = 16#FF,
    ConnectTimeout = make_resend_timer(ChannelID, SequenceNr, ?PEER_TIMEOUT_MINIMUM, Data),

    NewS = S#state{
             outgoing_reliable_sequence_number = SequenceNr + 1,
             connect_id = ConnectID,
             mtu = MTU
            },
    {keep_state, NewS, [ConnectTimeout]};

connecting(cast, {incoming_command, _SentTime, {H, C = #acknowledge{}}}, S) ->
    %%
    %% Received an Acknowledge command in the 'connecting' state.
    %%
    %% - Verify that the acknowledge is correct
    %% - Change state to 'acknowledging_verify_connect'
    %%

    ?DBG_PEER_HS(">>|| [~w] connecting ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #command_header{
      channel_id = ChannelID,
      reliable_sequence_number = SequenceNr
    } = H,
    CanceledTimeout = cancel_resend_timer(ChannelID, SequenceNr),

    {next_state, acknowledging_verify_connect, S, [CanceledTimeout]};

connecting(cast, {incoming_command, SentTime, {H, C = #verify_connect{}}}, S) ->
    ?DBG_PEER_HS(">>|| [~w] connecting ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #command_header{
      channel_id = ChannelID,
      reliable_sequence_number = SequenceNr
    } = H,
    CanceledTimeout = cancel_resend_timer(ChannelID, SequenceNr),

    gen_statem:cast(self(), {incoming_command, SentTime, {H, C}}),

    {next_state, acknowledging_verify_connect, S, [CanceledTimeout]};


connecting({timeout, {ChannelID, SequenceNr}}, Data, S) ->
    %%
    %% A resend-timer was triggered.
    %%
    %% - TODO: Keep track of number of resends
    %% - Resend the associated command
    %% - Reset the resend-timer
    %% - Reset the send-timer
    %%

    ?DBG_PEER_HS(">>|| [~w] connecting timeout ChID/SeqNr=~w ~n", [S#state.local_port, {ChannelID, SequenceNr}]),

    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID
      } = S,
    enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port),
    NewTimeout = make_resend_timer(ChannelID, SequenceNr, ?PEER_TIMEOUT_MINIMUM, Data),
    {keep_state, S, [NewTimeout]};

connecting({timeout, {_ChannelID, _SentTime, _SequenceNumber}}, _, S) ->
    {stop, timeout, S};

connecting(EventType, EventContent, S) ->
    handle_event(EventType, EventContent, S).


%%%
%%% Acknowledging Verify Connect state
%%%

acknowledging_verify_connect(enter, _OldState, S) ->
    {keep_state, S};

acknowledging_verify_connect(cast, {incoming_command, SentTime, {H, C = #verify_connect{}}}, S) ->
    %%
    %% Received a Verify Connect command in the 'acknowledging_verify_connect'
    %% state.
    %%
    %% - Verify that the data is correct
    %% - Add the remote peer ID to the Peer Table
    %% - Notify worker that we are connected
    %% - Change state to 'connected'
    %%

    %%
    %% TODO: Calculate and validate Session IDs
    %%

    ?DBG_PEER_HS(">>|| [~w] acknowledging_verify_connect ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #state{
      host = Host,
      remote_ip = FromIP,
      remote_port = Port,
      channel_count = LocalChannelCount,
      mtu = LocalMTU,
      window_size = LocalWindowSize
    } = S,

    #verify_connect{
       outgoing_peer_id             = RemotePeerID,
       incoming_session_id          = IncomingSessionID,
       outgoing_session_id          = OutgoingSessionID,
       mtu                          = _RemoteMTU,
       window_size                  = _RemoteWindowSize,
       channel_count                = RemoteChannelCount,
       incoming_bandwidth           = IncomingBandwidth,
       outgoing_bandwidth           = OutgoingBandwidth,
       packet_throttle_interval     = ThrottleInterval,
       packet_throttle_acceleration = ThrottleAcceleration,
       packet_throttle_deceleration = ThrottleDeceleration,
       connect_id                   = ConnectID
      } = C,

    case S of
        #state{
           %% ---
           %% Fields below are matched against the values received in
           %% the Verify Connect command.
           %% ---
           packet_throttle_interval     = ThrottleInterval,
           packet_throttle_acceleration = ThrottleAcceleration,
           packet_throttle_deceleration = ThrottleDeceleration,
           connect_id                   = ConnectID
           %% ---
          } when
          RemoteChannelCount >= ?MIN_CHANNEL_COUNT,
          RemoteChannelCount =< ?MAX_CHANNEL_COUNT
          ->
            ChannelCountSet = min(LocalChannelCount, RemoteChannelCount),

            MTU1 = enet_command:clamp(C#verify_connect.mtu, ?MAX_MTU, ?MIN_MTU),
            MTUSet = min(MTU1, LocalMTU),

            WindowSize1 = enet_command:clamp(C#verify_connect.window_size, ?MIN_WINDOW_SIZE, ?MAX_WINDOW_SIZE),
            WindowSizeSet = min(WindowSize1, LocalWindowSize),

            {AckH, AckC} = enet_command:acknowledge(H, SentTime),
            ?DBG_PEER_HS("<<|| [~w] acknowledging_verify_connect AckAfter: ~s ~w ~n", [S#state.local_port, enet_command:command_name(AckH), {AckH, AckC}]),

            HBin = enet_protocol_encode:command_header(AckH),
            CBin = enet_protocol_encode:command(AckC),
            Data = [HBin, CBin],
            {sent_time, _AckSentTime} =
                enet_host:send_outgoing_commands(Host, Data, ConnectID, OutgoingSessionID, FromIP, Port, RemotePeerID),

            NewS = S#state{
              channel_count = ChannelCountSet,
              remote_peer_id = RemotePeerID,
              incoming_session_id = IncomingSessionID,
              outgoing_session_id = OutgoingSessionID,
              mtu = MTUSet,
              window_size = WindowSizeSet,
              incoming_bandwidth = IncomingBandwidth,
              outgoing_bandwidth = OutgoingBandwidth
            },
            {next_state, connected, NewS};
        _Mismatch ->
            {stop, connect_verification_failed, S}
    end;

acknowledging_verify_connect(EventType, EventContent, S) ->
    handle_event(EventType, EventContent, S).


%%%
%%% Acknowledging Connect state
%%%

acknowledging_connect(enter, _OldState, S) ->
    {keep_state, S};

acknowledging_connect(cast, {incoming_command, _SentTime, {H, C = #connect{}}}, S) ->
    %%
    %% Received a Connect command.
    %%
    %% - Verify that the data is sane (TODO)
    %% - Send a VerifyConnect command (use peer ID)
    %% - Start in the 'verifying_connect' state
    %%

    ?DBG_PEER_HS(">>|| [~w] acknowledging_connect ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       local_peer_id = PeerID,
       incoming_session_id = IncomingSessionID,
       outgoing_session_id = OutgoingSessionID,
       outgoing_reliable_sequence_number = SequenceNr
    } = S,

    #connect{
       outgoing_peer_id             = RemotePeerID,
       incoming_session_id          = _ConnectIncomingSessionID,
       outgoing_session_id          = _ConnectOutgoingSessionID,
       mtu                          = _ConnectMTU,
       window_size                  = _ConnectWindowSize,
       channel_count                = ConnectChannelCount,
       incoming_bandwidth           = IncomingBandwidth,
       outgoing_bandwidth           = OutgoingBandwidth,
       packet_throttle_interval     = PacketThrottleInterval,
       packet_throttle_acceleration = PacketThrottleAcceleration,
       packet_throttle_deceleration = PacketThrottleDeceleration,
       connect_id                   = ConnectID,
       data                         = _Data
      } = C,

      case S of
          #state{} when
            ConnectChannelCount >= ?MIN_CHANNEL_COUNT,
            ConnectChannelCount =< ?MAX_CHANNEL_COUNT
            ->
              HostChannelLimit = enet_host:get_channel_limit(Host),
              HostIncomingBandwidth = enet_host:get_incoming_bandwidth(Host),
              HostOutgoingBandwidth = enet_host:get_outgoing_bandwidth(Host),

              ChannelCountSet = min(C#connect.channel_count, HostChannelLimit),
              IncomingSessionIDSet = enet_command:calculate_session_id(C#connect.incoming_session_id, OutgoingSessionID),
              OutgoingSessionIDSet = enet_command:calculate_session_id(C#connect.outgoing_session_id, IncomingSessionID),
              MTUSet = enet_command:clamp(C#connect.mtu, ?MAX_MTU, ?MIN_MTU),
              WindowSizeSet = enet_command:calculate_window_size(IncomingBandwidth, C#connect.window_size),

              {VCH, VCC} = enet_command:verify_connect(
                                         PeerID,
                                         OutgoingSessionIDSet,
                                         IncomingSessionIDSet,
                                         MTUSet,
                                         WindowSizeSet,
                                         ChannelCountSet,
                                         HostIncomingBandwidth,
                                         HostOutgoingBandwidth,
                                         PacketThrottleInterval,
                                         PacketThrottleAcceleration,
                                         PacketThrottleDeceleration,
                                         ConnectID,
                                         SequenceNr),

              ?DBG_PEER_HS("<<|| [~w] acknowledging_connect ~s ~w ~n", [S#state.local_port, enet_command:command_name(VCH), {VCH, VCC}]),

              HBin = enet_protocol_encode:command_header(VCH),
              CBin = enet_protocol_encode:command(VCC),
              Data = [HBin, CBin],
              {sent_time, SentTime} =
                  enet_host:send_outgoing_commands(Host, Data, ConnectID, IncomingSessionIDSet, IP, Port, RemotePeerID),
              ChannelID = 16#FF,
              VerifyConnectTimeout =
                  make_resend_timer(
                    ChannelID, SentTime, SequenceNr, ?PEER_TIMEOUT_MINIMUM, Data),

              NewS = S#state{
                channel_count = ChannelCountSet,
                connect_id = ConnectID,
                remote_peer_id = RemotePeerID,
                incoming_bandwidth = IncomingBandwidth,
                outgoing_bandwidth = OutgoingBandwidth,
                packet_throttle_interval = PacketThrottleInterval,
                packet_throttle_acceleration = PacketThrottleAcceleration,
                packet_throttle_deceleration = PacketThrottleDeceleration,

                incoming_session_id = OutgoingSessionIDSet,
                outgoing_session_id = IncomingSessionIDSet,
                mtu = MTUSet,
                window_size = WindowSizeSet,
                outgoing_reliable_sequence_number = SequenceNr + 1
              },
              {next_state, verifying_connect, NewS, [VerifyConnectTimeout]};
          _Invalid ->
              {stop, connect_channel_count_invalid, S}
      end;

acknowledging_connect({timeout, {_ChannelID, _SentTime, _SequenceNr}}, _, S) ->
    {stop, timeout, S};

acknowledging_connect(EventType, EventContent, S) ->
    handle_event(EventType, EventContent, S).


%%%
%%% Verifying Connect state
%%%

verifying_connect(enter, _OldState, S) ->
    {keep_state, S};

verifying_connect(cast, {incoming_command, _SentTime, {H, C = #acknowledge{}}}, S) ->
    %%
    %% Received an Acknowledge command in the 'verifying_connect' state.
    %%
    %% - Verify that the acknowledge is correct
    %% - Notify worker that a new peer has been connected
    %% - Change to 'connected' state
    %%

    ?DBG_PEER_HS(">>|| [~w] verifying_connect ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #command_header{ channel_id = ChannelID } = H,
    #acknowledge{
       received_reliable_sequence_number = SequenceNumber,
       received_sent_time                = SentTime
      } = C,
    CanceledTimeout = cancel_resend_timer(ChannelID, SentTime, SequenceNumber),
    {next_state, connected, S, [CanceledTimeout]};

verifying_connect(EventType, EventContent, S) ->
    handle_event(EventType, EventContent, S).


%%%
%%% Connected state
%%%

connected(enter, _OldState, S) ->
    #state{
       local_port = LocalPort,
       remote_ip = IP,
       remote_port = Port,
       mtu = MTU,
       remote_peer_id = RemotePeerID,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       channel_count = N,
       connect_fun = ConnectFun
      } = S,
    true = gproc:mreg(p, l, [
                             {mtu, MTU},
                             {connect_id, ConnectID},
                             {outgoing_session_id, SessionID},
                             {remote_host_port, Port},
                             {remote_peer_id, RemotePeerID}
                            ]),
    ok = enet_disconnector:set_trigger(LocalPort, RemotePeerID, IP, Port, ConnectID, SessionID),
    Channels = start_channels(N),
    PeerInfo = #{
                 peer => self(),
                 channels => Channels,
                 connect_id => ConnectID,
                 session_id => SessionID,
                 remote_ip => IP,
                 remote_port => Port
                },
    case start_worker(ConnectFun, PeerInfo) of
        {error, Reason} ->
            {stop, {worker_init_error, Reason}, S};
        {ok, Worker} ->
            _Ref = monitor(process, Worker),
            [enet_channel:set_worker(C, Worker) || C <- maps:values(Channels)],
            NewS = S#state{
                     channels = Channels,
                     worker = Worker
                    },
            SendTimeout = reset_send_timer(),
            RecvTimeout = reset_recv_timer(),
            {keep_state, NewS, [SendTimeout, RecvTimeout]}
    end;

connected(cast, {incoming_command, _SentTime, {H, C=#verify_connect{}}}, S) ->
    %%
    %% Received VERYFY_CONNECT again.
    %%
    %% - Reset the receive-timer
    %%

    ?DBG_PEER(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    RecvTimeout = reset_recv_timer(),
    {keep_state, S, [RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C=#ping{}}}, S) ->
    %%
    %% Received PING.
    %%
    %% - Reset the receive-timer
    %%

    ?DBG_MISC(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    RecvTimeout = reset_recv_timer(),
    {keep_state, S, [RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C = #acknowledge{}}}, S) ->
    %%
    %% Received an Acknowledge command.
    %%
    %% - Verify that the acknowledge is correct
    %% - Reset the receive-timer
    %%

    ?DBG_MISC(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #command_header{ channel_id = ChannelID } = H,
    #acknowledge{
       received_reliable_sequence_number = SequenceNumber,
       received_sent_time                = SentTime
      } = C,
    CanceledTimeout = cancel_resend_timer(ChannelID, SentTime, SequenceNumber),
    RecvTimeout = reset_recv_timer(),

    {PRoundTripTime, PRoundTripTimeVariance} = calc_roundTripTime(SentTime, S),

    {keep_state, S#state{roundTripTime = PRoundTripTime, roundTripTimeVariance = PRoundTripTimeVariance}, [CanceledTimeout, RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C = #bandwidth_limit{}}}, S) ->
    %%
    %% Received Bandwidth Limit command.
    %%
    %% - Set bandwidth limit
    %% - Reset the receive-timer
    %%

    ?DBG_MISC(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #bandwidth_limit{
       incoming_bandwidth = IncomingBandwidth,
       outgoing_bandwidth = OutgoingBandwidth
      } = C,
    #state{ host = Host } = S,
    HostOutgoingBandwidth = enet_host:get_outgoing_bandwidth(Host),
    WSize =
        case {IncomingBandwidth, HostOutgoingBandwidth} of
            {0, 0} -> ?MAX_WINDOW_SIZE;
            {0, H} -> ?MIN_WINDOW_SIZE *         H div ?PEER_WINDOW_SIZE_SCALE;
            {P, 0} -> ?MIN_WINDOW_SIZE *         P div ?PEER_WINDOW_SIZE_SCALE;
            {P, H} -> ?MIN_WINDOW_SIZE * min(P, H) div ?PEER_WINDOW_SIZE_SCALE
        end,
    NewS = S#state{
             incoming_bandwidth = IncomingBandwidth,
             outgoing_bandwidth = OutgoingBandwidth,
             window_size = max(?MIN_WINDOW_SIZE, min(?MAX_WINDOW_SIZE, WSize))
            },
    RecvTimeout = reset_recv_timer(),
    {keep_state, NewS, [RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C = #throttle_configure{}}}, S) ->
    %%
    %% Received Throttle Configure command.
    %%
    %% - Set throttle configuration
    %% - Reset the receive-timer
    %%

    ?DBG_MISC(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #throttle_configure{
       packet_throttle_interval = Interval,
       packet_throttle_acceleration = Acceleration,
       packet_throttle_deceleration = Deceleration
      } = C,
    NewS = S#state{
             packet_throttle_interval = Interval,
             packet_throttle_acceleration = Acceleration,
             packet_throttle_deceleration = Deceleration
            },
    RecvTimeout = reset_recv_timer(),
    {keep_state, NewS, [RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C = #unsequenced{}}}, S) ->
    %%
    %% Received Send Unsequenced command.
    %%
    %% - Forward the command to the relevant channel
    %% - Reset the receive-timer
    %%

    ?DBG_PEER(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #command_header{ channel_id = ChannelID } = H,
    #state{ channels = #{ ChannelID := Channel } } = S,
    ok = enet_channel:recv_unsequenced(Channel, {H, C}),
    RecvTimeout = reset_recv_timer(),
    {keep_state, S, [RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C = #unreliable{}}}, S) ->
    %%
    %% Received Send Unreliable command.
    %%
    %% - Forward the command to the relevant channel
    %% - Reset the receive-timer
    %%

    ?DBG_PEER(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #command_header{ channel_id = ChannelID } = H,
    #state{ channels = #{ ChannelID := Channel } } = S,
    ok = enet_channel:recv_unreliable(Channel, {H, C}),
    RecvTimeout = reset_recv_timer(),
    {keep_state, S, [RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C = #fragment{}}}, S) ->
    %%
    %% Received Send Fragment command.
    %%
    %% - Forward the command to the relevant channel
    %% - Reset the receive-timer
    %%

    ?DBG_PEER(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #command_header{ channel_id = ChannelID } = H,
    #state{ channels = #{ ChannelID := Channel } } = S,
    ok = enet_channel:recv_reliable(Channel, {H, C}),
    RecvTimeout = reset_recv_timer(),
    {keep_state, S, [RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C = #reliable{}}}, S) ->
    %%
    %% Received Send Reliable command.
    %%
    %% - Forward the command to the relevant channel
    %% - Reset the receive-timer
    %%

    ?DBG_PEER(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #command_header{ channel_id = ChannelID } = H,
    #state{ channels = #{ ChannelID := Channel } } = S,
    ok = enet_channel:recv_reliable(Channel, {H, C}),
    RecvTimeout = reset_recv_timer(),
    {keep_state, S, [RecvTimeout]};

connected(cast, {incoming_command, _SentTime, {H, C=#disconnect{}}}, S) ->
    %%
    %% Received Disconnect command.
    %%
    %% - Notify worker application
    %% - Stop
    %%

    ?DBG_PEER(">> [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #state{
       worker = Worker,
       local_port = LocalPort,
       remote_ip = IP,
       remote_port = Port,
       remote_peer_id = RemotePeerID,
       connect_id = ConnectID,
       outgoing_session_id = SessionID
      } = S,
    ok = enet_disconnector:unset_trigger(LocalPort, RemotePeerID, IP, Port, ConnectID, SessionID),
    Worker ! {enet, disconnected, remote, self(), ConnectID, SessionID},
    {stop, normal, S};

connected(cast, {outgoing_command, {H, C = #unsequenced{}}}, S) ->
    %%
    %% Sending an Unsequenced, unreliable command.
    %%
    %% - TODO: Increment total data passed through peer
    %% - Increment outgoing_unsequenced_group
    %% - Set unsequenced_group on command to outgoing_unsequenced_group
    %% - Queue the command for sending
    %% - Reset the send-timer
    %%
    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       remote_peer_id = RemotePeerID,
       outgoing_unsequenced_group = Group
      } = S,
    C1 = C#unsequenced{ group = Group },
    HBin = enet_protocol_encode:command_header(H),
    CBin = enet_protocol_encode:command(C1),
    Data = [HBin, CBin],
    {sent_time, _SentTime} =
        enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID),
    NewS = S#state{ outgoing_unsequenced_group = Group + 1 },
    SendTimeout = reset_send_timer(),
    {keep_state, NewS, [SendTimeout]};

connected(cast, {outgoing_command, {H, C = #unreliable{}}}, S) ->
    %%
    %% Sending a Sequenced, unreliable command.
    %%
    %% - Forward the encoded command to the host
    %% - Reset the send-timer
    %%

    ?DBG_PEER("<< [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       remote_peer_id = RemotePeerID
      } = S,
    HBin = enet_protocol_encode:command_header(H),
    CBin = enet_protocol_encode:command(C),
    Data = [HBin, CBin],
    {sent_time, _SentTime} =
        enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID),
    SendTimeout = reset_send_timer(),
    {keep_state, S, [SendTimeout]};

connected(cast, {send_reliable_outgoing_commands, Commands}, S) ->
    lists:foreach(fun({H, C}) ->
      enet_peer:send_command(self(), {H, C})
    end, Commands),

    {keep_state, S};

connected(cast, {outgoing_command, {H, C = #reliable{}}}, S) ->
    %%
    %% Sending a Sequenced, reliable command.
    %%
    %% - Forward the encoded command to the host
    %% - Reset the send-timer
    %%

    ?DBG_PEER("<< [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       remote_peer_id = RemotePeerID
      } = S,
    #command_header{
       channel_id = ChannelID,
       reliable_sequence_number = SequenceNr
      } = H,
    HBin = enet_protocol_encode:command_header(H),
    CBin = enet_protocol_encode:command(C),
    Data = [HBin, CBin],
    {sent_time, SentTime} =
        enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID),
    RoundTripTimeout = S#state.roundTripTime + 4 * S#state.roundTripTimeVariance,
    ?DBG_RESENT("send #reliable RoundTripTimeout=~w ~n", [RoundTripTimeout]),
    SendReliableTimeout =
        make_resend_timer(
          ChannelID, SentTime, SequenceNr, RoundTripTimeout, {Data, RoundTripTimeout}),
    SendTimeout = reset_send_timer(),
    {keep_state, S, [SendReliableTimeout, SendTimeout]};

connected(cast, {outgoing_command, {H, C = #fragment{}}}, S) ->
    %%
    %% Sending a Sequenced, fragment command.
    %%
    %% - Forward the encoded command to the host
    %% - Reset the send-timer
    %%

    ?DBG_PEER("<< [~w] connected ~s ~w ~n", [S#state.local_port, enet_command:command_name(H), {H, C}]),

    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       remote_peer_id = RemotePeerID
      } = S,
    #command_header{
       channel_id = ChannelID,
       reliable_sequence_number = SequenceNr
      } = H,
    HBin = enet_protocol_encode:command_header(H),
    CBin = enet_protocol_encode:command(C),
    Data = [HBin, CBin],
    {sent_time, SentTime} =
        enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID),
    RoundTripTimeout = S#state.roundTripTime + 4 * S#state.roundTripTimeVariance,
    ?DBG_RESENT("send #fragment RoundTripTimeout=~w ~n", [RoundTripTimeout]),
    SendReliableTimeout =
        make_resend_timer(
          ChannelID, SentTime, SequenceNr, RoundTripTimeout, {Data, RoundTripTimeout}),
    SendTimeout = reset_send_timer(),
    {keep_state, S, [SendReliableTimeout, SendTimeout]};

connected(cast, disconnect, State) ->
    %%
    %% Disconnecting.
    %%
    %% - Queue a Disconnect command for sending
    %% - Change state to 'disconnecting'
    %%
    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       remote_peer_id = RemotePeerID
      } = State,

    {H, C} = enet_command:sequenced_disconnect(),
    ?DBG_PEER("<< [~w] connected ~s ~w ~n", [State#state.local_port, enet_command:command_name(H), {H, C}]),

    HBin = enet_protocol_encode:command_header(H),
    CBin = enet_protocol_encode:command(C),
    Data = [HBin, CBin],
    enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID),
    {next_state, disconnecting, State};

connected(cast, disconnect_now, State) ->
    %%
    %% Disconnecting immediately.
    %%
    %% - Stop
    %%
    {stop, normal, State};

connected({timeout, {ChannelID, SentTime, SequenceNr}}, {Data, RoundTripTimeout}, S) ->
    %%
    %% A resend-timer was triggered.
    %%
    %% - TODO: Keep track of number of resends
    %% - Resend the associated command
    %% - Reset the resend-timer
    %% - Reset the send-timer
    %%

    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       remote_peer_id = RemotePeerID
      } = S,

    ?DBG_RESENT("<< [~w] connected resendxx ~w ~n", [S#state.local_port, {ChannelID, SentTime, SequenceNr}]),

    {sent_time, NewSentTime} = enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID),
    NewRoundTripTimeout = RoundTripTimeout*2,
    NewTimeout =
        make_resend_timer(
          ChannelID, NewSentTime, SequenceNr, NewRoundTripTimeout, {Data, NewRoundTripTimeout}),
    SendTimeout = reset_send_timer(),
    {keep_state, S, [NewTimeout, SendTimeout]};

connected({timeout, {ChannelID, SentTime, SequenceNr}}, Data, S) ->
    %%
    %% A resend-timer was triggered.
    %%
    %% - TODO: Keep track of number of resends
    %% - Resend the associated command
    %% - Reset the resend-timer
    %% - Reset the send-timer
    %%

    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       remote_peer_id = RemotePeerID
      } = S,

    ?DBG_RESENT("<< [~w] connected resend ~w ~n", [S#state.local_port, {ChannelID, SentTime, SequenceNr}]),

    enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID),
    NewTimeout =
        make_resend_timer(
          ChannelID, SentTime, SequenceNr, ?PEER_TIMEOUT_MINIMUM, Data),
    SendTimeout = reset_send_timer(),
    {keep_state, S, [NewTimeout, SendTimeout]};

connected({timeout, recv}, ping, #state{}=S) ->
    %%
    %% The receive-timer was triggered.
    %%
    %% - Stop
    %%

    {stop, timeout, S};

connected({timeout, send}, ping, S) ->
    %%
    %% The send-timer was triggered.
    %%
    %% - Send a PING
    %% - Reset the send-timer
    %%

    #state{
       host = Host,
       remote_ip = IP,
       remote_port = Port,
       connect_id = ConnectID,
       outgoing_session_id = SessionID,
       remote_peer_id = RemotePeerID,
       outgoing_reliable_sequence_number = SequenceNr
      } = S,

    {H, C} = enet_command:ping(SequenceNr),

    HBin = enet_protocol_encode:command_header(H),
    CBin = enet_protocol_encode:command(C),
    Data = [HBin, CBin],
    {sent_time, _SentTime} =
        enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID),
    SendTimeout = reset_send_timer(),
    NewS = S#state{
         outgoing_reliable_sequence_number = SequenceNr + 1
        },
    {keep_state, NewS, [SendTimeout]};

connected(EventType, EventContent, #state{}=S) ->
    handle_event(EventType, EventContent, S).


%%%
%%% Disconnecting state
%%%

disconnecting(enter, _OldState, S) ->
    #state{
       worker = Worker,
       local_port = LocalPort,
       remote_ip = IP,
       remote_port = Port,
       remote_peer_id = RemotePeerID,
       connect_id = ConnectID,
       outgoing_session_id = SessionID
      } = S,
    ok = enet_disconnector:unset_trigger(LocalPort, RemotePeerID, IP, Port, ConnectID, SessionID),
    Worker ! {enet, disconnected, local, self(), ConnectID, SessionID},
    {stop, normal, S};

    % {keep_state, S};

disconnecting(cast, {incoming_command, _SentTime, {_H, _C = #acknowledge{}}}, S) ->
    %%
    %% Received an Acknowledge command in the 'disconnecting' state.
    %%
    %% - Verify that the acknowledge is correct
    %% - Notify worker application
    %% - Stop
    %%
    #state{
       worker = Worker,
       connect_id = ConnectID,
       outgoing_session_id = SessionID
      } = S,
    Worker ! {enet, disconnected, local, self(), ConnectID, SessionID},
    {stop, normal, S};

disconnecting(cast, {incoming_command, _SentTime, {_H, _C}}, S) ->
    {keep_state, S};

disconnecting(EventType, EventContent, S) ->
    handle_event(EventType, EventContent, S).


%%%
%%% terminate
%%%

terminate(_Reason, _StateName, #state{ local_port = LocalPort }) ->
    Name = get_name(self()),
    enet_pool:disconnect_peer(LocalPort, Name),
    ok.


%%%
%%% code_change
%%%

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_event(cast, {incoming_packet, _FromIP, SentTime, Packet}, S) ->
    %%
    %% Received an incoming packet of commands.
    %%
    %% - Split and decode the commands from the binary
    %% - Send the commands as individual events to ourselves
    %%
    #state{
      host = Host,
      remote_ip = IP,
      remote_port = Port,
      connect_id = ConnectID,
      outgoing_session_id = SessionID
    } = S,

    {ok, Commands} = enet_protocol_decode:commands(Packet),
    lists:foreach(
      fun ({H = #command_header{ please_acknowledge = 0 }, C}) ->
              %%
              %% Received command that does not need to be acknowledged.
              %%
              %% - Send the command to self for handling
              %%

              % ?DBG_PEER(">> [~w] ~w ~n", [S#state.local_port, {H, C}]),

              gen_statem:cast(self(), {incoming_command, SentTime, {H, C}});
          ({H = #command_header{ please_acknowledge = 1 }, C}) ->
              %%
              %% Received a command that should be acknowledged.
              %%
              %% - Acknowledge the command
              %% - Send the command to self for handling
              %%

              % ?DBG_PEER(">> [~w] ~w ~n", [S#state.local_port, {H, C}]),

              {AckNow, RemotePeerID} =
                  case C of
                      #connect{}        -> {false, C#connect.outgoing_peer_id};
                      #verify_connect{} -> {false, C#verify_connect.outgoing_peer_id};
                      _                 -> {true, S#state.remote_peer_id}
                  end,

              case AckNow of
                true ->
                  {AckH, AckC} = enet_command:acknowledge(H, SentTime),
                  ?DBG_MISC("<< [~w] AckNow: ~w ~n", [S#state.local_port, {AckH, AckC}]),

                  HBin = enet_protocol_encode:command_header(AckH),
                  CBin = enet_protocol_encode:command(AckC),
                  Data = [HBin, CBin],
                  {sent_time, _AckSentTime} =
                      enet_host:send_outgoing_commands(Host, Data, ConnectID, SessionID, IP, Port, RemotePeerID);
                _ -> ok
              end,

              gen_statem:cast(self(), {incoming_command, SentTime, {H, C}})
      end,
      Commands),
    {keep_state, S};

handle_event({call, From}, channels, S) ->
    {keep_state, S, [{reply, From, S#state.channels}]};

handle_event({call, From}, pool, S) ->
    {keep_state, S, [{reply, From, S#state.local_port}]};

handle_event({call, From}, pool_worker_id, S) ->
    WorkerID = enet_pool:worker_id(S#state.local_port, get_name(self())),
    {keep_state, S, [{reply, From, WorkerID}]};

handle_event({call, From}, {channel, ID}, S) ->
    #state{ channels = #{ ID := Channel }} = S,
    {keep_state, S, [{reply, From, Channel}]};

handle_event(info, {'DOWN', _, process, O, _}, S = #state{ worker = O }) ->
    {stop, worker_process_down, S}.


start_channels(N) ->
    IDs = lists:seq(0, N-1),
    Channels =
        lists:map(
          fun (ID) ->
                  {ok, Channel} = enet_channel:start_link(ID, self()),
                  {ID, Channel}
          end,
          IDs),
    maps:from_list(Channels).

make_resend_timer(ChannelID, SequenceNumber, Time, Data) ->
    {{timeout, {ChannelID, SequenceNumber}}, Time, Data}.

make_resend_timer(ChannelID, SentTime, SequenceNumber, Time, Data) ->
    {{timeout, {ChannelID, SentTime, SequenceNumber}}, Time, Data}.

cancel_resend_timer(ChannelID, SequenceNumber) ->
    {{timeout, {ChannelID, SequenceNumber}}, infinity, undefined}.

cancel_resend_timer(ChannelID, SentTime, SequenceNumber) ->
    {{timeout, {ChannelID, SentTime, SequenceNumber}}, infinity, undefined}.

reset_recv_timer() ->
    % {{timeout, recv}, 2 * ?PEER_PING_INTERVAL, ping}.
    {{timeout, recv}, infinity, undefined}.

reset_send_timer() ->
    {{timeout, send}, ?PEER_PING_INTERVAL, ping}.

start_worker({Module, Fun, Args}, PeerInfo) ->
    erlang:apply(Module, Fun, [PeerInfo] ++ Args);
start_worker(ConnectFun, PeerInfo) ->
    ConnectFun(PeerInfo).

calc_roundTripTime(SentTime, S) ->
  ServiceTime = get_time(),
  ReceivedSentTime1 = SentTime bor (ServiceTime band 16#FFFF0000),
  ReceivedSentTime = if
    (ReceivedSentTime1 band 16#8000) > (ServiceTime band 16#8000) ->
      ReceivedSentTime1 - 16#10000;
    true ->
      ReceivedSentTime1
  end,

  if
    ?ENET_TIME_LESS(ServiceTime, ReceivedSentTime) ->
      ?DBG_RESENT("?ENET_TIME_LESS(ServiceTime, ReceivedSentTime) FAILED ~n");
    true ->
      ?DBG_RESENT("?ENET_TIME_LESS(ServiceTime, ReceivedSentTime) OK ~n")
  end,

  RoundTripTime = ?ENET_TIME_DIFFERENCE (ServiceTime, ReceivedSentTime),
  ?DBG_RESENT("RoundTripTime ~w peer->roundTripTime ~w peer->roundTripTimeVariance ~w ~n", [RoundTripTime, S#state.roundTripTime, S#state.roundTripTimeVariance]),

  PRoundTripTimeVariance1 = S#state.roundTripTimeVariance - (S#state.roundTripTimeVariance div 4),
  {PRoundTripTime, PRoundTripTimeVariance} = if
    RoundTripTime >= S#state.roundTripTime ->
      PRoundTripTime11 = S#state.roundTripTime + ((RoundTripTime - S#state.roundTripTime) div 8),
      PRoundTripTimeVariance12 = PRoundTripTimeVariance1 + ((RoundTripTime - PRoundTripTime11) div 4),
      {PRoundTripTime11, PRoundTripTimeVariance12};
    true ->
      PRoundTripTime21 = S#state.roundTripTime - ((S#state.roundTripTime - RoundTripTime) div 8),
      PRoundTripTimeVariance22 = PRoundTripTimeVariance1 + ((PRoundTripTime21 - RoundTripTime) div 4),
      {PRoundTripTime21, PRoundTripTimeVariance22}
  end,

  {PRoundTripTime, PRoundTripTimeVariance}.

get_time() ->
    erlang:system_time(1000) band 16#FFFF.

