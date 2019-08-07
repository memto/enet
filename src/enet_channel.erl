-module(enet_channel).

-include("enet_commands.hrl").

-export([
         start_link/2,
         stop/1,
         set_worker/2,
         recv_unsequenced/2,
         send_unsequenced/2,
         recv_unreliable/2,
         send_unreliable/2,
         recv_reliable/2,
         send_reliable/2
        ]).

-export([
         init/3
        ]).

-export([
         system_code_change/4,
         system_continue/3,
         system_terminate/4,
         write_debug/3
        ]).

-record(state,
        {
          id,
          peer,
          worker,
          incoming_reliable_sequence_number = 1,
          incoming_unreliable_sequence_number = 0,
          outgoing_reliable_sequence_number = 1,
          outgoing_unreliable_sequence_number = 1,
          reliable_fragment_command = nil,
          reliable_windows, %% reliableWindows [ENET_PEER_RELIABLE_WINDOWS] (uint16 * 16 = 32 bytes)
          used_reliable_windows = 0,
          sys_parent,
          sys_debug
        }).


%%%
%%% API
%%%

start_link(ID, Peer) ->
    proc_lib:start_link(?MODULE, init, [ID, Peer, self()]).

stop(Channel) ->
    Channel ! stop.

set_worker(Channel, Worker) ->
    Channel ! {set_worker, Worker}.

recv_unsequenced(Channel, {H, C}) ->
    %% Peer -> Channel -> Worker
    Channel ! {recv_unsequenced, {H, C}},
    ok.

send_unsequenced(Channel, Data) ->
    %% Worker -> Channel -> Peer
    Channel ! {send_unsequenced, Data},
    ok.

recv_unreliable(Channel, {H, C}) ->
    %% Peer -> Channel -> Worker
    Channel ! {recv_unreliable, {H, C}},
    ok.

send_unreliable(Channel, Data) ->
    %% Worker -> Channel -> Peer
    Channel ! {send_unreliable, Data},
    ok.

recv_reliable(Channel, {H, C}) ->
    %% Peer -> Channel -> Worker
    Channel ! {recv_reliable, {H, C}},
    ok.

send_reliable(Channel, Data) ->
    %% Worker -> Channel -> Peer
    Channel ! {send_reliable, Data},
    ok.


%%%
%%% Implementation
%%%

init(ID, Peer, Parent) ->
    Debug = sys:debug_options([]),
    State = #state{
               id = ID,
               peer = Peer,
               sys_parent = Parent,
               sys_debug = Debug
              },
    proc_lib:init_ack(Parent, {ok, self()}),
    await_worker(State).


await_worker(S) ->
    receive
        {set_worker, Worker} -> loop(S#state{ worker = Worker })
    end.


loop(S = #state{ id = ID, peer = Peer, worker = Worker }) ->
    receive
        {system, From, Request} ->
            #state{ sys_parent = Parent, sys_debug = Debug } = S,
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, S);

        {recv_unsequenced, {
           #command_header{ unsequenced = 1 },
           C = #unsequenced{}
          }} ->
            Worker ! {enet, ID, C},
            loop(S);

        {send_unsequenced, Data} ->
            {H, C} = enet_command:send_unsequenced(ID, Data),
            ok = enet_peer:send_command(Peer, {H, C}),
            loop(S);

        {recv_unreliable, {
           #command_header{},
           C = #unreliable{ sequence_number = N }
          }} ->
            if N < S#state.incoming_unreliable_sequence_number ->
                    %% Data is old - drop it and continue.
                    loop(S);
               true ->
                    Worker ! {enet, ID, C},
                    NewS = S#state{ incoming_unreliable_sequence_number = N },
                    loop(NewS)
            end;

        {send_unreliable, Data} ->
            RN = S#state.outgoing_reliable_sequence_number - 1,
            N = S#state.outgoing_unreliable_sequence_number,
            {H, C} = enet_command:send_unreliable(ID, RN, N, Data),
            % io:fwrite(" << send_unreliable ~w ~n", [{H, C}]),
            ok = enet_peer:send_command(Peer, {H, C}),
            NewS = S#state{ outgoing_unreliable_sequence_number = N + 1 },
            loop(NewS);

        {recv_reliable, {
           #command_header{ reliable_sequence_number = N },
           C = #reliable{}
          }} when N =:= S#state.incoming_reliable_sequence_number ->
            Worker ! {enet, ID, C},
            NewS = S#state{ incoming_reliable_sequence_number = N + 1, incoming_unreliable_sequence_number = 1},
            loop(NewS);

        {recv_reliable, {
           #command_header{ reliable_sequence_number = N },
           C = #fragment{}
          }} when N =:= S#state.incoming_reliable_sequence_number ->
            {FullCmd, S1} = join_data(recv_reliable, C, S),
            NewS = case FullCmd of
              nil ->
                S1#state{ incoming_reliable_sequence_number = N + 1 };
              _ ->
                Worker ! {enet, ID, FullCmd},
                S1#state{ incoming_reliable_sequence_number = N + 1, incoming_unreliable_sequence_number = 1 }
            end,
            loop(NewS);

        {send_reliable, Data} ->
            {Commands, NewS} = split_data(send_reliable, Data, S),
            enet_peer:send_reliable_outgoing_commands(Peer, Commands),
            loop(NewS#state{outgoing_unreliable_sequence_number = 1});
        stop ->
            stopped
    end.


% start_sequence_number = StartSN,
% data_length           = DataLen,
% fragment_count        = FragCnt,
% fragment_number       = FragNum,
% total_length          = TotalLen,
% fragment_offset       = FragOff,
% data                  = Data
join_data(recv_reliable, C = #fragment{}, #state{reliable_fragment_command = RFC}=S) ->
  RFC1 = case RFC of
    nil ->
      FragmentData = C#fragment.data,
      FillDataLen = (C#fragment.total_length - byte_size(FragmentData))*8,
      CmdData = <<FragmentData/binary, 0:FillDataLen>>,
      C#fragment{data=CmdData, fragment_count=C#fragment.fragment_count-1};
    StartCmd ->
      if
        C#fragment.start_sequence_number =:= StartCmd#fragment.start_sequence_number ->
          #fragment{
            fragment_offset = FragmentOff,
            data = FragmentData
          } = C,
          DataLen = byte_size(FragmentData),
          <<Data1:FragmentOff/binary, _:DataLen/binary, Rest/binary>> = StartCmd#fragment.data,
          CmdData = <<Data1:FragmentOff/binary, FragmentData/binary, Rest/binary>>,
          StartCmd#fragment{data=CmdData, fragment_count=StartCmd#fragment.fragment_count-1};
        true -> new_start_sequence_number
      end
  end,

  case RFC1#fragment.fragment_count of
    0 ->
      {#reliable{data = RFC1#fragment.data}, S#state{reliable_fragment_command=nil}};
    _ ->
      {nil, S#state{reliable_fragment_command=RFC1}}
  end.

split_data(send_reliable, Data, #state{id = ID}=S) ->
  PeerMTU = enet_peer:get_mtu(S#state.peer),

  FragLen1 = PeerMTU -
  (?PROTOCOL_HEADER_SIZE + ?PROTOCOL_COMMAND_HEADER_SIZE + ?PROTOCOL_SEND_FRAGMENT_SIZE),
  FragLenX = case ?CRC32 of
    true -> FragLen1 - 4;
    _ -> FragLen1
  end,

  TotalLen = byte_size(Data),
  if
    TotalLen > FragLenX ->
      StartSN = S#state.outgoing_reliable_sequence_number,
      FragCnt = (TotalLen + FragLenX - 1) div FragLenX,
      lists:foldl(fun(X, ACC) ->
        {Commands1, S1} = ACC,
        N = S1#state.outgoing_reliable_sequence_number,

        FragNum = X,
        FragOff = FragNum * FragLenX,
        DataLen = min(FragLenX, TotalLen - FragOff),
        <<_:FragOff/binary, DataX:DataLen/binary, _Rest/binary>> = Data,

        % send_fragment(ChannelID, ReliableSequenceNumber, StartSN, DataLen, FragCnt, FragNum, TotalLen, FragOff, Data) ->
        Cmd = enet_command:send_fragment(ID, N, StartSN, DataLen, FragCnt, FragNum, TotalLen, FragOff, DataX),

        NewCmds = Commands1 ++ [Cmd],
        NewS = S1#state{outgoing_reliable_sequence_number = N + 1},

        {NewCmds, NewS}
      end, {[], S}, lists:seq(0, FragCnt-1));
    true ->
      N = S#state.outgoing_reliable_sequence_number,
      {H, C} = enet_command:send_reliable(ID, N, Data),
      % io:fwrite(" << send_reliable ~w ~n", [{H, C}]),
      Commands = [{H, C}],
      NewS = S#state{ outgoing_reliable_sequence_number = N + 1 },
      {Commands, NewS}
  end.



%%%
%%% System message handling
%%%

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

system_continue(_Parent, _Debug, State) ->
    loop(State).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.
