-module(enet_host).
-behaviour(gen_server).

-include("enet_peer.hrl").
-include("enet_commands.hrl").
-include("enet_protocol.hrl").
-include("enet_compress.hrl").
-include("enet_helpers.hrl").

%% API
-export([
         start_link/4,
         socket_options/0,
         give_socket/2,
         connect/5,
         send_outgoing_commands/6,
         send_outgoing_commands/7,
         get_port/1,
         get_incoming_bandwidth/1,
         get_outgoing_bandwidth/1,
         get_mtu/1,
         get_channel_limit/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(NULL_PEER_ID, ?MAX_PEER_ID).

-record(state,
        {
         socket,
         connect_fun,
         compressor,
         transport = gen_udp,
         connect_id = undefined
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, ConnectFun, Commpressor, Options) ->
    gen_server:start_link(?MODULE, {Port, ConnectFun, Commpressor, Options}, []).

socket_options() ->
    [binary, {active, false}, {reuseaddr, false}, {broadcast, true}].

give_socket(Host, Socket) ->
    ok = gen_udp:controlling_process(Socket, Host),
    gen_server:cast(Host, {give_socket, Socket}).

connect(Host, RIP, RPort, ChannelCount, Proxy) ->
    gen_server:call(Host, {connect, RIP, RPort, ChannelCount, Proxy}).

send_outgoing_commands(Host, Commands, ConnectID, SessionID, RIP, RPort) ->
    send_outgoing_commands(Host, Commands, ConnectID, SessionID, RIP, RPort, ?NULL_PEER_ID).

send_outgoing_commands(Host, Commands, ConnectID, SessionID, RIP, RPort, RPeerID) ->
    gen_server:call(Host, {send_outgoing_commands, Commands, ConnectID, SessionID, RIP, RPort, RPeerID}).

get_port(Host) ->
    gproc:get_value({p, l, port}, Host).

get_incoming_bandwidth(Host) ->
    gproc:get_value({p, l, incoming_bandwidth}, Host).

get_outgoing_bandwidth(Host) ->
    gproc:get_value({p, l, outgoing_bandwidth}, Host).

get_mtu(Host) ->
    gproc:get_value({p, l, mtu}, Host).

get_channel_limit(Host) ->
    gproc:get_value({p, l, channel_limit}, Host).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({AssignedPort, ConnectFun, Commpressor, Options}) ->
    true = gproc:reg({n, l, {enet_host, AssignedPort}}),
    ChannelLimit =
        case lists:keyfind(channel_limit, 1, Options) of
            {channel_limit, CLimit} -> CLimit;
            false                   -> ?MIN_CHANNEL_COUNT
        end,
    IncomingBandwidth =
        case lists:keyfind(incoming_bandwidth, 1, Options) of
            {incoming_bandwidth, IBandwidth} -> IBandwidth;
            false                            -> 0
        end,
    OutgoingBandwidth =
        case lists:keyfind(outgoing_bandwidth, 1, Options) of
            {outgoing_bandwidth, OBandwidth} -> OBandwidth;
            false                            -> 0
        end,
    true = gproc:mreg(p, l,
                      [
                       {port, AssignedPort},
                       {channel_limit, ChannelLimit},
                       {incoming_bandwidth, IncomingBandwidth},
                       {outgoing_bandwidth, OutgoingBandwidth},
                       {mtu, ?HOST_DEFAULT_MTU}
                      ]),
    case gen_udp:open(AssignedPort, socket_options()) of
        {error, eaddrinuse} ->
            %%
            %% A socket has already been opened on this port
            %% - The socket will be given to us later
            %%
            {ok, #state{ connect_fun = ConnectFun, compressor = Commpressor }};
        {ok, Socket} ->
            %%
            %% We were able to open a new socket on this port
            %% - It means we have been restarted by the supervisor
            %% - Set it to active mode
            %%
            ok = inet:setopts(Socket, [{active, true}]),
            {ok, #state{ connect_fun = ConnectFun, compressor = Commpressor, socket = Socket }}
    end.


handle_call({connect, IP, Port, Channels, undefined}, _From, S) ->
    %%
    %% Connect to a remote peer.
    %%
    %% - Add a peer to the pool
    %% - Start the peer process
    %%
    #state{
       connect_fun = ConnectFun
      } = S,
    Ref = make_ref(),
    LPort = get_port(self()),
    Reply =
        try enet_pool:add_peer(LPort, Ref) of
            PeerID ->
                Peer = #enet_peer{
                          handshake_flow = local,
                          local_peer_id = PeerID,
                          remote_ip = IP,
                          remote_port = Port,
                          name = Ref,
                          host = self(),
                          channels = Channels,
                          connect_fun = ConnectFun
                         },
                start_peer(Peer)
        catch
            error:pool_full -> {error, reached_peer_limit};
            error:exists    -> {error, exists}
        end,
    {reply, Reply, S#state{transport=gen_udp}};


handle_call({connect, IP, Port, Channels, Proxy}, _From, S) ->
    %%
    %% Connect to a remote peer.
    %%
    %% - Add a peer to the pool
    %% - Start the peer process
    %%
    #state{
       connect_fun = ConnectFun,
       socket = Socket
      } = S,
    Ref = make_ref(),
    LPort = get_port(self()),
    {ok, Socket2} = socks5_udp:connect(IP, Port, Socket, Proxy),
    Reply =
        try enet_pool:add_peer(LPort, Ref) of
            PeerID ->
                Peer = #enet_peer{
                          handshake_flow = local,
                          local_peer_id = PeerID,
                          remote_ip = IP,
                          remote_port = Port,
                          name = Ref,
                          host = self(),
                          channels = Channels,
                          connect_fun = ConnectFun
                         },
                start_peer(Peer)
        catch
            error:pool_full -> {error, reached_peer_limit};
            error:exists    -> {error, exists}
        end,
    {reply, Reply, S#state{transport=socks5_udp, socket=Socket2}};

handle_call({send_outgoing_commands, Commands, ConnectID, SessionID, RIP, RPort, RPeerID}, _From, S) ->
    %%
    %% Received outgoing commands from a peer.
    %%
    %% - Compress commands if compressor available (TODO)
    %% - Wrap the commands in a protocol header
    %% - Send the packet
    %% - Return sent time
    %%

    SentTime = get_time(),

    #state{
       socket = Socket,
       transport = Transport
      } = S,

    PH = if
      RPeerID < ?MAX_PEER_ID ->
        #protocol_header{
          session_id = SessionID,
          peer_id = RPeerID,
          sent_time = SentTime
         };
      true ->
        #protocol_header{
          peer_id = RPeerID,
          sent_time = SentTime
         }
    end,

    ?DBG_HOST("<< ~w ~n", [PH]),

    PH_Checksum = case ?CRC32 of
      false ->
        enet_protocol_encode:protocol_header(PH);
      true ->
        PHBin = enet_protocol_encode:protocol_header(PH),
        CommandsBin = case is_list(Commands) of
          true ->
            binary:list_to_bin(Commands);
          false ->
            Commands
        end,

        Payload =
        if
          (RPeerID < ?MAX_PEER_ID) ->
            ?DBG_CHECKSUM("Checksum Remote Peer ConnectID=~w ~n", [ConnectID]),
            <<PHBin/binary, ConnectID:32, CommandsBin/binary>>;
          true ->
            ?DBG_CHECKSUM("Checksum No Remote Peer ~n"),
            <<PHBin/binary, 0:32, CommandsBin/binary>>
        end,

        ?DBG_CHECKSUM("Checksum input: ~w ~n", [Payload]),
        Checksum = erlang:crc32(Payload),
        <<PHBin/binary, Checksum:32>>
    end,

    Packet = [PH_Checksum, Commands],
  
    ?DBG_PACKET("<< send udp ~w ~n", [Packet]),  
    ok = Transport:send(Socket, RIP, RPort, Packet),
  
    {reply, {sent_time, SentTime}, S#state{connect_id=ConnectID}}.


%%%
%%% handle_cast
%%%

handle_cast({give_socket, Socket}, S) ->
    ok = inet:setopts(Socket, [{active, true}]),
    {noreply, S#state{ socket = Socket }};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%%
%%% handle_info
%%%

handle_info({udp, Socket, RIP, RPort, Packet}, #state{transport = gen_udp} = S) ->
    %%
    %% Received a UDP packet.
    %%
    %% - Unpack the ENet protocol header
    %% - Decompress the remaining packet if necessary
    %% - Send the packet to the peer (ID in protocol header)
    %%


    #state{
       socket = Socket,
       connect_fun = ConnectFun,
       compressor = #enet_compressor{
        context = Context,
        compress = _CompressFun,
        decompress = DecompressFun,
        destroy = _DestroyFun
       },
       connect_id = ConnectID
      } = S,
    %% TODO: Replace call to enet_protocol_decode with binary pattern match.

    ?DBG_PACKET(">> revc udp: ~w ~n", [Packet]),

    {ok,
     #protocol_header{
        compressed = IsCompressed,
        peer_id = LPeerID,
        sent_time = SentTime
       }=PH,
     Rest1} = enet_protocol_decode:protocol_header(Packet),

    ?DBG_HOST("~n>> ~w ~n", [PH]),

    Rest = case ?CRC32 of
      false ->
        Rest1;
      true ->
        <<_:32, Rest2/binary>> = Rest1,
        Rest2
    end,


    Commands =
        case IsCompressed of
            0 -> Rest;
            1 ->
              {ok, Out} = start_compressor(DecompressFun, Context, Rest),
              Out
        end,

    _Checksum_ok = case ?CRC32 of
      true ->
        <<RemoteChecksum:32, _/binary>> = Rest1,

        PHBin = enet_protocol_encode:protocol_header(PH),
        Payload = case LPeerID of
          ?MAX_PEER_ID ->
            <<PHBin/binary, 0:32, Commands/binary>>;
          _ ->
            <<PHBin/binary, ConnectID:32, Commands/binary>>
        end,
        Checksum = erlang:crc32(Payload),

        ?DBG_CHECKSUM("Checksum input: ~w ~n", [Payload]),
        ?DBG_CHECKSUM("Checksum/RemoteChecksum: ~w/~w  LPeerID/ConnectID=~w/~w ~n", [Checksum, RemoteChecksum, LPeerID, ConnectID]),
        Checksum == RemoteChecksum;
      false ->
        true
    end,

    LPort = get_port(self()),
    case LPeerID of
        ?MAX_PEER_ID ->
            %% No particular peer is the receiver of this packet.
            %% Create a new peer.
            Ref = make_ref(),
            try enet_pool:add_peer(LPort, Ref) of
                PeerID ->
                  Peer = #enet_peer{
                            handshake_flow = remote,
                            local_peer_id = PeerID,
                            remote_ip = RIP,
                            remote_port = RPort,
                            name = Ref,
                            host = self(),
                            connect_fun = ConnectFun
                           },
                  {ok, Pid} = start_peer(Peer),

                  enet_peer:recv_incoming_packet(Pid, RIP, SentTime, Commands)
            catch
                error:pool_full -> {error, reached_peer_limit};
                error:exists    -> {error, exists}
            end;
        LPeerID ->
            case enet_pool:pick_peer(LPort, LPeerID) of
                false -> ok; %% Unknown peer - drop the packet
                Pid ->
                  enet_peer:recv_incoming_packet(Pid, RIP, SentTime, Commands)
            end
    end,
    {noreply, S};

handle_info({udp, _Socket, RIP, RPort, Packet}, S) ->
    %%
    %% Received a UDP packet.
    %%
    %% - Unpack the ENet protocol header
    %% - Decompress the remaining packet if necessary
    %% - Send the packet to the peer (ID in protocol header)
    %%


    #state{
       connect_fun = ConnectFun,
       compressor = #enet_compressor{
        context = Context,
        compress = _CompressFun,
        decompress = DecompressFun,
        destroy = _DestroyFun
       },
       connect_id = ConnectID,
       transport = Transport
      } = S,
    %% TODO: Replace call to enet_protocol_decode with binary pattern match.

    Packet1 = Transport:remove_header(Packet),

    {ok,
     #protocol_header{
        compressed = IsCompressed,
        peer_id = LPeerID,
        sent_time = SentTime
       }=PH,
     Rest1} = enet_protocol_decode:protocol_header(Packet1),

    Rest = case ?CRC32 of
      false ->
        Rest1;
      true ->
        <<_:32, Rest2/binary>> = Rest1,
        Rest2
    end,


    Commands =
        case IsCompressed of
            0 -> Rest;
            1 ->
              {ok, Out} = start_compressor(DecompressFun, Context, Rest),
              Out
        end,

    _Checksum_ok = case ?CRC32 of
      true ->
        <<RemoteChecksum:32, _/binary>> = Rest1,

        PHBin = enet_protocol_encode:protocol_header(PH),
        Payload = case LPeerID of
          ?MAX_PEER_ID ->
            <<PHBin/binary, 0:32, Commands/binary>>;
          _ ->
            <<PHBin/binary, ConnectID:32, Commands/binary>>
        end,
        Checksum = erlang:crc32(Payload),

        Checksum == RemoteChecksum;
      false ->
        true
    end,

    LPort = get_port(self()),
    case LPeerID of
        ?MAX_PEER_ID ->
            %% No particular peer is the receiver of this packet.
            %% Create a new peer.
            Ref = make_ref(),
            try enet_pool:add_peer(LPort, Ref) of
                PeerID ->
                  Peer = #enet_peer{
                            handshake_flow = remote,
                            local_peer_id = PeerID,
                            remote_ip = RIP,
                            remote_port = RPort,
                            name = Ref,
                            host = self(),
                            connect_fun = ConnectFun
                           },
                  {ok, Pid} = start_peer(Peer),

                  enet_peer:recv_incoming_packet(Pid, RIP, SentTime, Commands)
            catch
                error:pool_full -> {error, reached_peer_limit};
                error:exists    -> {error, exists}
            end;
        LPeerID ->
            case enet_pool:pick_peer(LPort, LPeerID) of
                false -> ok; %% Unknown peer - drop the packet
                Pid ->
                  enet_peer:recv_incoming_packet(Pid, RIP, SentTime, Commands)
            end
    end,
    {noreply, S};

handle_info({gproc, unreg, _Ref, {n, l, {enet_peer, Ref}}}, S) ->
    %%
    %% A Peer process has exited.
    %%
    %% - Remove it from the pool
    %%
    LPort = get_port(self()),
    true = enet_pool:remove_peer(LPort, Ref),
    {noreply, S}.


%%%
%%% terminate
%%%

terminate(_Reason, #state{socket = Socket, transport = Transport}) ->
    ok = Transport:close(Socket).


%%%
%%% code_change
%%%

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_time() ->
    erlang:system_time(1000) band 16#FFFF.

start_peer(Peer = #enet_peer{ name = Ref }) ->
    LPort = gproc:get_value({p, l, port}, self()),
    PeerSup = gproc:where({n, l, {enet_peer_sup, LPort}}),
    {ok, Pid} = enet_peer_sup:start_peer(PeerSup, Peer),
    _Ref = gproc:monitor({n, l, {enet_peer, Ref}}),
    {ok, Pid}.

start_compressor({Module, Fun, Args}, Context, Payload) ->
    erlang:apply(Module, Fun, [Context, Payload] ++ Args);
start_compressor(Fun, Context, Payload) ->
    Fun(Context, Payload).
