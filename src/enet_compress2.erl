-module(enet_compress2).

-include("enet_peer.hrl").
-include("enet_commands.hrl").
-include("enet_protocol.hrl").
-include("enet_compress2.hrl").

-export([
         enet_get_compressor/1,
         enet_range_coder_compress/2,
         enet_range_coder_decompress/2,
         enet_range_coder_destroy/1
        ]).


enet_get_compressor(range_coder) ->
  #enet_compressor {
    context = enet_range_coder_create(),
    compress = {enet_compress2, enet_range_coder_compress, []},
    decompress = {enet_compress2, enet_range_coder_decompress, []},
    destroy = {enet_compress2, enet_range_coder_destroy, []}
  }.


enet_range_coder_create() ->
  orddict:from_list([{Idx, #enet_symbol{}} || Idx <- lists:seq(0,4095)]).
  % orddict:from_list([{Idx, #enet_symbol{}} || Idx <- lists:seq(0,4)]).


enet_range_coder_compress(Context, Payload) ->
  io:fwrite("enet_range_coder_compress ~n"),
    EnetRcCompVars = #enet_rc_comp_vars{
    encodeLow = 0,
    encodeRange = ?MAX_UINT32,

    predicted = 0,
    order = 0,
    nextSymbol = 0
  },

  % {Context1, EnetRcCompVars1} = enet_context_create(Context, EnetRcCompVars, ?ENET_CONTEXT_ESCAPE_MINIMUM, ?ENET_CONTEXT_SYMBOL_MINIMUM),
  % Commands = do_enet_range_coder_compress(EnetRcCompVars1, Payload),

  % Commands.
  {ok, <<>>}.

do_enet_range_coder_compress(Context, EnetRcCompVars, Payload) ->
  % Maybe somthing here to handle this
  % inData = (const enet_uint8 *) inBuffers -> data;
  % inEnd = & inData [inBuffers -> dataLength];
  % inBuffers ++;
  % inBufferCount --;

  EnetRcCompLoop1Vars = #enet_rc_dec_loop1_vars{
    subcontext = 0,
    symbol = 0,
    patch = 0,

    value = 0,
    code = 0,
    under = 0,
    count = 0,
    bottom = 0,
    parent = -1,
    total = 0,

    decodeNode = 0,
    encodeNode = 0
  },

  enet_range_coder_compress_loop1(Context, EnetRcCompVars, EnetRcCompLoop1Vars, Payload, <<>>),

  % Maybe somthing here to handle this
  % ENET_RANGE_CODER_FLUSH;
  ok.


enet_range_coder_compress_loop1(Context, EnetRcCompVars, EnetRcCompLoop1Vars, Payload, Output) ->
  % enet_range_coder_compress_loop1_top,
  % enet_range_coder_compress_loop1_middle,
  % enet_range_coder_compress_loop1_bottom
  ok.

enet_range_coder_compress_loop1_top(Context, EnetRcCompVars, EnetRcCompLoop1Vars, Payload, Output) ->
  ok.

enet_range_coder_compress_loop1_middle(Context, EnetRcCompVars, EnetRcCompLoop1Vars, Payload, Output) ->
  % enet_range_coder_compress_loop1_middle_loop1

  % ENET_CONTEXT_ENCODE (root, symbol, value, under, count, ENET_CONTEXT_SYMBOL_DELTA, ENET_CONTEXT_SYMBOL_MINIMUM);
  % * parent = symbol - rangeCoder -> symbols;
  % parent = & symbol -> parent;
  % total = root -> total;
  % ENET_RANGE_CODER_ENCODE (root -> escapes + under, count, total);
  % root -> total += ENET_CONTEXT_SYMBOL_DELTA;
  % if (count > 0xFF - 2*ENET_CONTEXT_SYMBOL_DELTA + ENET_CONTEXT_SYMBOL_MINIMUM || root -> total > ENET_RANGE_CODER_BOTTOM - 0x100)
  %   ENET_CONTEXT_RESCALE (root, ENET_CONTEXT_SYMBOL_MINIMUM);
  ok.

enet_range_coder_compress_loop1_middle_loop1(Context, EnetRcCompVars, EnetRcCompLoop1Vars, Payload, Output) ->
    % for (subcontext = & rangeCoder -> symbols [predicted];
    %      subcontext != root;
    %         subcontext = & rangeCoder -> symbols [subcontext -> parent])
    % {
    %     ENET_CONTEXT_ENCODE (subcontext, symbol, value, under, count, ENET_SUBCONTEXT_SYMBOL_DELTA, 0);
    %     * parent = symbol - rangeCoder -> symbols;
    %     parent = & symbol -> parent;
    %     total = subcontext -> total;
    %     if (count > 0)
    %     {
    %         ENET_RANGE_CODER_ENCODE (subcontext -> escapes + under, count, total);
    %     }
    %     else
    %     {
    %         if (subcontext -> escapes > 0 && subcontext -> escapes < total)
    %             ENET_RANGE_CODER_ENCODE (0, subcontext -> escapes, total);
    %         subcontext -> escapes += ENET_SUBCONTEXT_ESCAPE_DELTA;
    %         subcontext -> total += ENET_SUBCONTEXT_ESCAPE_DELTA;
    %     }
    %     subcontext -> total += ENET_SUBCONTEXT_SYMBOL_DELTA;
    %     if (count > 0xFF - 2*ENET_SUBCONTEXT_SYMBOL_DELTA || subcontext -> total > ENET_RANGE_CODER_BOTTOM - 0x100)
    %       ENET_CONTEXT_RESCALE (subcontext, 0);
    %     if (count > 0) goto nextInput;
    % }
    ok.

enet_range_coder_compress_loop1_bottom(Context, EnetRcCompVars, EnetRcCompLoop1Vars, Payload, Output) ->
    % nextInput:
    %     if (order >= ENET_SUBCONTEXT_ORDER)
    %       predicted = rangeCoder -> symbols [predicted].parent;
    %     else
    %       order ++;
    %     ENET_RANGE_CODER_FREE_SYMBOLS;
  ok.


enet_range_coder_decompress(Context, Payload) ->
  io:fwrite("~w enet_range_coder_decompress ~n", [?MODULE]),

  EnetRcDecVars = #enet_rc_dec_vars{
    symbols = Context,

    decodeLow = 0,
    decodeCode = 0,
    decodeRange = ?MAX_UINT32,

    predicted = 0,
    order = 0,
    nextSymbol = 0
  },

  EnetRcDecVars1 = enet_context_create(EnetRcDecVars, ?ENET_CONTEXT_ESCAPE_MINIMUM, ?ENET_CONTEXT_SYMBOL_MINIMUM),
  {EnetRcDecVars2, Payload1} = enet_range_coder_seed(EnetRcDecVars1, Payload),
  Commands = do_enet_range_coder_decompress(EnetRcDecVars2, Payload1),

  {ok, Commands}.


do_enet_range_coder_decompress(EnetRcDecVars, Payload) ->
  EnetRcDecLoop1Vars = #enet_rc_dec_loop1_vars{
    subcontext = 0,
    symbol = 0,
    patch = 0,

    value = 0,
    code = 0,
    under = 0,
    count = 0,
    bottom = 0,
    parent = -1,
    total = 0,

    decodeNode = 0,
    encodeNode = 0
  },
  enet_range_coder_decompress_loop1(EnetRcDecVars, EnetRcDecLoop1Vars, Payload, <<>>).


enet_range_coder_decompress_loop1(EnetRcDecVars, EnetRcDecLoop1Vars, Payload, Output) ->
  EnetRcDecLoop1Vars_x = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{subcontext = EnetRcDecVars#enet_rc_dec_vars.predicted},
  {EnetRcDecVars1, EnetRcDecLoop1Vars1, Payload1, Out1, GotoPatchEnetRcDecVarss} =
    enet_range_coder_decompress_loop1_top(EnetRcDecVars, EnetRcDecLoop1Vars_x, Payload, Output),

  {EnetRcDecVars2, EnetRcDecLoop1Vars2, Payload2, Out2, Break} = case GotoPatchEnetRcDecVarss of
    false ->
      enet_range_coder_decompress_loop1_middle(EnetRcDecVars1, EnetRcDecLoop1Vars1, Payload1, Out1);
    _ ->
      {EnetRcDecVars1, EnetRcDecLoop1Vars1, Payload1, Out1, false}
    end,

  Out3 = case Break of
    true ->
      Out2;
    _ ->
      EnetRcDecLoop1Vars_2 = EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars{patch = EnetRcDecVars2#enet_rc_dec_vars.predicted},
      {EnetRcDecVars_1, _EnetRcDecLoop1Vars_1, Payload_1, Out_1} = enet_range_coder_decompress_loop1_bottom(EnetRcDecVars2, EnetRcDecLoop1Vars_2, Payload2, Out2),
      NewEnetRcDecLoop1Vars = #enet_rc_dec_loop1_vars{
        subcontext = 0,
        symbol = 0,
        patch = 0,

        value = 0,
        code = 0,
        under = 0,
        count = 0,
        bottom = 0,
        parent = -1,
        total = 0,

        decodeNode = 0
      },
      enet_range_coder_decompress_loop1(EnetRcDecVars_1, NewEnetRcDecLoop1Vars, Payload_1, Out_1)
    end,
  Out3
  .


enet_range_coder_decompress_loop1_top(EnetRcDecVars, EnetRcDecLoop1Vars, Payload, Output) ->
  SubcontextIdx = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.subcontext,
  {EnetRcDecVars1, EnetRcDecLoop1Vars1, Payload1, Output1, GotoPatchEnetRcDecVarss} =
  if
    SubcontextIdx == 0 ->
      {EnetRcDecVars, EnetRcDecLoop1Vars, Payload, Output, false};
    true ->
      SubEnetRcDecVarsSymbol = orddict:fetch(SubcontextIdx, EnetRcDecVars#enet_rc_dec_vars.symbols),
      if
        SubEnetRcDecVarsSymbol#enet_symbol.escapes =< 0 ->
          EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{subcontext = SubEnetRcDecVarsSymbol#enet_symbol.parent},
          enet_range_coder_decompress_loop1_top(EnetRcDecVars, EnetRcDecLoop1Vars_1, Payload, Output);
        true ->
          EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{total = SubEnetRcDecVarsSymbol#enet_symbol.total},
          if
            SubEnetRcDecVarsSymbol#enet_symbol.escapes >= EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.total ->
              EnetRcDecLoop1Vars__1 = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars{subcontext = SubEnetRcDecVarsSymbol#enet_symbol.parent},
              enet_range_coder_decompress_loop1_top(EnetRcDecVars, EnetRcDecLoop1Vars__1, Payload, Output);
            true ->
              {EnetRcDecVars__1, Code1}  = enet_range_coder_read(EnetRcDecVars, EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.total),
              EnetRcDecLoop1Vars__1 = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars{code = Code1},
              if
                EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars.code < SubEnetRcDecVarsSymbol#enet_symbol.escapes ->
                  {EnetRcDecVars___1, Payload___1} = enet_range_coder_decode(EnetRcDecVars__1, 0, SubEnetRcDecVarsSymbol#enet_symbol.escapes, EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars.total, Payload),
                  EnetRcDecLoop1Vars___1 = EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars{subcontext = SubEnetRcDecVarsSymbol#enet_symbol.parent},
                  enet_range_coder_decompress_loop1_top(EnetRcDecVars___1, EnetRcDecLoop1Vars___1, Payload___1, Output);
                true ->
                  Code2 = EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars.code - SubEnetRcDecVarsSymbol#enet_symbol.escapes,
                  EnetRcDecLoop1Vars___1 = EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars{code = Code2},

                  {EnetRcDecVars___1, EnetRcDecLoop1Vars___2} = enet_context_try_decode(EnetRcDecVars__1, EnetRcDecLoop1Vars___1, ?ENET_SUBCONTEXT_SYMBOL_DELTA, 0),


                  EnetRcDecLoop1Vars___3 = EnetRcDecLoop1Vars___2#enet_rc_dec_loop1_vars{bottom = EnetRcDecLoop1Vars___2#enet_rc_dec_loop1_vars.symbol},

                  {EnetRcDecVars___2, Payload___1} = enet_range_coder_decode(EnetRcDecVars___1,
                    SubEnetRcDecVarsSymbol#enet_symbol.escapes + EnetRcDecLoop1Vars___3#enet_rc_dec_loop1_vars.under,
                    EnetRcDecLoop1Vars___3#enet_rc_dec_loop1_vars.count, EnetRcDecLoop1Vars___3#enet_rc_dec_loop1_vars.total,
                    Payload),
                  Symbols1 = orddict:update(SubcontextIdx, fun(OldSymbol) ->
                    OldSymbol#enet_symbol{
                      total = OldSymbol#enet_symbol.total + ?ENET_SUBCONTEXT_SYMBOL_DELTA
                    }
                  end, EnetRcDecVars___2#enet_rc_dec_vars.symbols),
                  EnetRcDecVars___3 = EnetRcDecVars___2#enet_rc_dec_vars{symbols = Symbols1},

                  SubEnetRcDecVarsSymbol_1 = orddict:fetch(SubcontextIdx, EnetRcDecVars___3#enet_rc_dec_vars.symbols),
                  XXX = EnetRcDecLoop1Vars___3#enet_rc_dec_loop1_vars.count > (255 - 2 * ?ENET_SUBCONTEXT_SYMBOL_DELTA),
                  YYY = SubEnetRcDecVarsSymbol_1#enet_symbol.total > ?ENET_RANGE_CODER_BOTTOM - 256,
                  if
                    XXX or YYY ->
                      EnetRcDecVars___4 = enet_context_rescale(EnetRcDecVars___3, SubcontextIdx, 0),
                      {EnetRcDecVars___4, EnetRcDecLoop1Vars___3, Payload___1, Output, true};
                    true ->
                      {EnetRcDecVars___3, EnetRcDecLoop1Vars___3, Payload___1, Output, true}
                  end
              end
          end
      end
  end,
  {EnetRcDecVars1, EnetRcDecLoop1Vars1, Payload1, Output1, GotoPatchEnetRcDecVarss}.


enet_range_coder_decompress_loop1_middle(EnetRcDecVars, EnetRcDecLoop1Vars, Payload, Output) ->
  Root1 = orddict:fetch(0, EnetRcDecVars#enet_rc_dec_vars.symbols),
  EnetRcDecLoop1Vars1_x = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{total = Root1#enet_symbol.total},

  Root2 = orddict:fetch(0, EnetRcDecVars#enet_rc_dec_vars.symbols),


  {EnetRcDecVars1, Code1} = enet_range_coder_read(EnetRcDecVars, EnetRcDecLoop1Vars1_x#enet_rc_dec_loop1_vars.total),
  EnetRcDecLoop1Vars1 = EnetRcDecLoop1Vars1_x#enet_rc_dec_loop1_vars{code = Code1},


  {EnetRcDecVars2, Payload1, Break} = if
    Code1 < Root2#enet_symbol.escapes ->
      {EnetRcDecVars_1, Payload_1} = enet_range_coder_decode(EnetRcDecVars1, 0, Root2#enet_symbol.escapes, EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.total, Payload),
      {EnetRcDecVars_1, Payload_1, true};
    true ->
      {EnetRcDecVars1, Payload, false}
  end,

  {EnetRcDecVars3, EnetRcDecLoop1Vars2, Payload2, Output1, Break2} = if
    Break == true ->
      {EnetRcDecVars2, EnetRcDecLoop1Vars1, Payload1, Output, Break};
    true ->
      Root3 = orddict:fetch(0, EnetRcDecVars2#enet_rc_dec_vars.symbols),
      EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars{code = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.code - Root3#enet_symbol.escapes},
      {EnetRcDecVars__1, EnetRcDecLoop1Vars_2} = enet_context_root_decode(EnetRcDecVars2, EnetRcDecLoop1Vars_1, ?ENET_CONTEXT_SYMBOL_DELTA, ?ENET_CONTEXT_SYMBOL_MINIMUM),
      EnetRcDecLoop1Vars_3 = EnetRcDecLoop1Vars_2#enet_rc_dec_loop1_vars{bottom = EnetRcDecLoop1Vars_2#enet_rc_dec_loop1_vars.symbol},
      Root4 = orddict:fetch(0, EnetRcDecVars__1#enet_rc_dec_vars.symbols),
      {EnetRcDecVars_2, Payload__1} = enet_range_coder_decode(EnetRcDecVars__1,
        Root4#enet_symbol.escapes + EnetRcDecLoop1Vars_3#enet_rc_dec_loop1_vars.under,
        EnetRcDecLoop1Vars_3#enet_rc_dec_loop1_vars.count, EnetRcDecLoop1Vars_3#enet_rc_dec_loop1_vars.total,
        Payload1),

      Symbols1 = orddict:update(0, fun(OldSymbol) ->
        OldSymbol#enet_symbol{total = OldSymbol#enet_symbol.total + ?ENET_CONTEXT_SYMBOL_DELTA}
      end, EnetRcDecVars_2#enet_rc_dec_vars.symbols),
      EnetRcDecVars_3 = EnetRcDecVars_2#enet_rc_dec_vars{symbols = Symbols1},
      Root5 = orddict:fetch(0, EnetRcDecVars_3#enet_rc_dec_vars.symbols),
      XXX = EnetRcDecLoop1Vars_3#enet_rc_dec_loop1_vars.count > (255 - 2 * ?ENET_CONTEXT_SYMBOL_DELTA + ?ENET_CONTEXT_SYMBOL_MINIMUM),
      YYY = Root5#enet_symbol.total > ?ENET_RANGE_CODER_BOTTOM - 256,
      if
        XXX or YYY ->
          EnetRcDecVars_4 = enet_context_rescale(EnetRcDecVars_3, 0, ?ENET_CONTEXT_SYMBOL_MINIMUM),
          {EnetRcDecVars_4, EnetRcDecLoop1Vars_3, Payload__1, Output, Break};
        true ->
          {EnetRcDecVars_3, EnetRcDecLoop1Vars_3, Payload__1, Output, Break}
      end
  end,
  {EnetRcDecVars3, EnetRcDecLoop1Vars2, Payload2, Output1, Break2}
  .


enet_range_coder_decompress_loop1_bottom(EnetRcDecVars, EnetRcDecLoop1Vars, Payload, Output) ->
  {EnetRcDecVars1, EnetRcDecLoop1Vars1, Payload1, Output1} = enet_range_coder_decompress_loop1_bottom_loop1(EnetRcDecVars, EnetRcDecLoop1Vars, Payload, Output),
  EnetRcDecVars2 = if
    EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.parent == -1 ->
      EnetRcDecVars1#enet_rc_dec_vars{predicted = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.bottom};
    true ->
      Symbols_1 = orddict:update(EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.parent, fun(OldSymbol) ->
        OldSymbol#enet_symbol{
          parent = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.bottom
        }
      end, EnetRcDecVars1#enet_rc_dec_vars.symbols),
      EnetRcDecVars1#enet_rc_dec_vars{symbols = Symbols_1}
  end,
  Val = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.value,
  ValBin = binary:encode_unsigned(Val),
  Output2 = <<Output1/binary, ValBin/binary>>,
  EnetRcDecVars3 = if
    EnetRcDecVars2#enet_rc_dec_vars.order >= ?ENET_SUBCONTEXT_ORDER ->
      Symbol_1 = orddict:fetch(EnetRcDecVars2#enet_rc_dec_vars.predicted, EnetRcDecVars2#enet_rc_dec_vars.symbols),
      EnetRcDecVars2#enet_rc_dec_vars{predicted = Symbol_1#enet_symbol.parent};
    true ->
      EnetRcDecVars2#enet_rc_dec_vars{order = EnetRcDecVars2#enet_rc_dec_vars.order + 1}
  end,
  EnetRcDecVars4 = enet_range_coder_free_symbols(EnetRcDecVars3),
  {EnetRcDecVars4, EnetRcDecLoop1Vars1, Payload1, Output2}
  .


enet_range_coder_free_symbols(EnetRcDecVars) ->
  if
    EnetRcDecVars#enet_rc_dec_vars.nextSymbol >= 4096 - ?ENET_SUBCONTEXT_ORDER ->
      EnetRcDecVars_1 = EnetRcDecVars#enet_rc_dec_vars{nextSymbol = 0},
      EnetRcDecVars_2 = enet_context_create(EnetRcDecVars_1, ?ENET_CONTEXT_ESCAPE_MINIMUM, ?ENET_CONTEXT_SYMBOL_MINIMUM),
      EnetRcDecVars_2#enet_rc_dec_vars{predicted = 0, order = 0};
    true ->
      EnetRcDecVars
  end
  .


enet_range_coder_decompress_loop1_bottom_loop1(EnetRcDecVars, EnetRcDecLoop1Vars, Payload, Output) ->
  PatchIdx = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.patch,
  SubcontextIdx = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.subcontext,
  {EnetRcDecVars1, EnetRcDecLoop1Vars1, Payload1, Output1} = if
      SubcontextIdx == PatchIdx ->
        {EnetRcDecVars, EnetRcDecLoop1Vars, Payload, Output};
      true ->
        {EnetRcDecVars_1, EnetRcDecLoop1Vars_1} = enet_context_encode(EnetRcDecVars, EnetRcDecLoop1Vars, PatchIdx, ?ENET_SUBCONTEXT_SYMBOL_DELTA, 0),
        EnetRcDecVars_2 = if
          EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.parent == -1 ->
            EnetRcDecVars_1#enet_rc_dec_vars{predicted = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.symbol};
          true ->
            Symbols_1 = orddict:update(EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.parent, fun(OldSymbol) ->
              OldSymbol#enet_symbol{
                parent = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.symbol
              }
            end, EnetRcDecVars_1#enet_rc_dec_vars.symbols),
            EnetRcDecVars_1#enet_rc_dec_vars{symbols = Symbols_1}
        end,
        EnetRcDecLoop1Vars__1 = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars{parent = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.symbol},
        PatchSymbol = orddict:fetch(PatchIdx, EnetRcDecVars_2#enet_rc_dec_vars.symbols),
        PatchSymbol_1 = if
          EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars.count =< 0 ->
            PatchSymbol#enet_symbol{
              escapes = PatchSymbol#enet_symbol.escapes + ?ENET_SUBCONTEXT_ESCAPE_DELTA,
              total = PatchSymbol#enet_symbol.total + ?ENET_SUBCONTEXT_ESCAPE_DELTA
            };
          true ->
            PatchSymbol
        end,
        PatchSymbol_2 = PatchSymbol_1#enet_symbol{total = PatchSymbol_1#enet_symbol.total + ?ENET_SUBCONTEXT_SYMBOL_DELTA},
        Symbols_2 = orddict:update(PatchIdx, fun(_OldSymbol) -> PatchSymbol_2 end, EnetRcDecVars_2#enet_rc_dec_vars.symbols),
        EnetRcDecVars_3 = EnetRcDecVars_2#enet_rc_dec_vars{symbols = Symbols_2},

        XXX = EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars.count > (255 - 2 * ?ENET_SUBCONTEXT_SYMBOL_DELTA),
        YYY = PatchSymbol_2#enet_symbol.total > ?ENET_RANGE_CODER_BOTTOM - 256,
        EnetRcDecVars_4 = if
          XXX or YYY ->
            enet_context_rescale(EnetRcDecVars_3, PatchIdx, 0);
          true ->
            EnetRcDecVars_3
        end,

        PatchSymbol_3 = orddict:fetch(PatchIdx, EnetRcDecVars_4#enet_rc_dec_vars.symbols),
        EnetRcDecLoop1Vars__2 = EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars{patch = PatchSymbol_3#enet_symbol.parent},
        enet_range_coder_decompress_loop1_bottom_loop1(EnetRcDecVars_4, EnetRcDecLoop1Vars__2, Payload, Output)
    end,
  {EnetRcDecVars1, EnetRcDecLoop1Vars1, Payload1, Output1}.


enet_range_coder_decode(EnetRcDecVars, Under, Count, _Total, Payload) ->
  DecodeLow1 = to_uint32(EnetRcDecVars#enet_rc_dec_vars.decodeLow + (Under * EnetRcDecVars#enet_rc_dec_vars.decodeRange)),
  DecodeRange1 = to_uint32(EnetRcDecVars#enet_rc_dec_vars.decodeRange * Count),
  EnetRcDecVars1 = EnetRcDecVars#enet_rc_dec_vars{decodeLow = DecodeLow1, decodeRange = DecodeRange1},
  {EnetRcDecVars2, Payload1} = enet_range_coder_decode_loop1(EnetRcDecVars1, Payload),
  {EnetRcDecVars2, Payload1}.


enet_range_coder_decode_loop1(EnetRcDecVars, Payload) ->
  #enet_rc_dec_vars{
    decodeLow = DecodeLow1,
    decodeRange = DecodeRange1,
    decodeCode = _DecodeCode1
  } = EnetRcDecVars,

  {EnetRcDecVars1, Break} = if
    (DecodeLow1 bxor (DecodeLow1 + DecodeRange1)) >= ?ENET_RANGE_CODER_TOP ->
      if
        DecodeRange1 >= ?ENET_RANGE_CODER_BOTTOM ->
          {EnetRcDecVars, true};
        true ->
          DecodeRange3 = to_uint32(-DecodeLow1 band (?ENET_RANGE_CODER_BOTTOM - 1)),
          {EnetRcDecVars#enet_rc_dec_vars{decodeRange = DecodeRange3}, false}
      end;
    true ->
      {EnetRcDecVars, false}
  end,

  {EnetRcDecVars2, Payload1} =
  case Break of
    false ->
        #enet_rc_dec_vars{
          decodeLow = DecodeLow_2,
          decodeRange = DecodeRange_2,
          decodeCode = DecodeCode_2
        } = EnetRcDecVars1,

      DecodeCode_3 = to_uint32(DecodeCode_2 bsl 8),
      {DecodeCode_4, Payload_1} = if
        byte_size(Payload) > 0 ->
          <<Val:8/unsigned-integer, Rest/binary>> = Payload,
          {to_uint32(DecodeCode_3 bor Val), Rest};
        true ->
          {DecodeCode_3, Payload}
      end,
      DecodeRange_3 = to_uint32(DecodeRange_2 bsl 8),
      DecodeLow_3 = to_uint32(DecodeLow_2 bsl 8),
      EnetRcDecVars_1 = EnetRcDecVars1#enet_rc_dec_vars{decodeLow = DecodeLow_3, decodeRange=DecodeRange_3, decodeCode = DecodeCode_4},
      enet_range_coder_decode_loop1(EnetRcDecVars_1, Payload_1);
    _ ->
      {EnetRcDecVars1, Payload}
  end,
  {EnetRcDecVars2, Payload1}.


enet_symbol_rescale(EnetRcDecVars, SymbolIdx, Total) ->
  Symbol = orddict:fetch(SymbolIdx, EnetRcDecVars#enet_rc_dec_vars.symbols),
  Count1 = Symbol#enet_symbol.count - (Symbol#enet_symbol.count bsr 1),
  Symbol1 = Symbol#enet_symbol{count = Count1, under = Count1},
  {EnetRcDecVars1, Under1} = if
    Symbol1#enet_symbol.left /= 0 ->
      Symbols1 = orddict:update(SymbolIdx, fun(_OldSymbol) -> Symbol1 end, EnetRcDecVars#enet_rc_dec_vars.symbols),
      EnetRcDecVars_1 = EnetRcDecVars#enet_rc_dec_vars{symbols = Symbols1},
      enet_symbol_rescale(EnetRcDecVars_1, SymbolIdx + Symbol1#enet_symbol.left, Total);
    true ->
      {EnetRcDecVars, 0}
  end,
  Total1 = Total + Under1,
  Symbol2 = Symbol1#enet_symbol{under = Symbol1#enet_symbol.under + Under1},
  {EnetRcDecVars2, Total2} = if
    Symbol2#enet_symbol.right == 0 ->
      {EnetRcDecVars1, Total1};
    true ->
      enet_symbol_rescale(EnetRcDecVars1, SymbolIdx + Symbol2#enet_symbol.right, Total1)
  end,
  {EnetRcDecVars2, Total2}.


enet_context_rescale(EnetRcDecVars, SymbolIdx, Minimum) ->
  Symbol = orddict:fetch(SymbolIdx, EnetRcDecVars#enet_rc_dec_vars.symbols),

  {EnetRcDecVars1, Total} = if
    Symbol#enet_symbol.symbols /= 0 ->
      enet_symbol_rescale(EnetRcDecVars, SymbolIdx + Symbol#enet_symbol.symbols, 0);
    true ->
      {EnetRcDecVars, 0}
  end,
  Symbol1 = Symbol#enet_symbol{total = Total},
  Escapes = Symbol1#enet_symbol.escapes - (Symbol1#enet_symbol.escapes bsr 1),
  Symbol2 = Symbol1#enet_symbol{escapes = Escapes},
  Total2 = Symbol2#enet_symbol.total + Symbol2#enet_symbol.escapes + 256 * Minimum,
  Symbol3 = Symbol2#enet_symbol{total = Total2},
  Symbols1 = orddict:update(SymbolIdx, fun(_OldSymbol) -> Symbol3 end, EnetRcDecVars1#enet_rc_dec_vars.symbols),
  EnetRcDecVars1#enet_rc_dec_vars{symbols = Symbols1}.


enet_range_coder_read(EnetRcDecVars, Total) ->
  DR1 = to_uint32(EnetRcDecVars#enet_rc_dec_vars.decodeRange div Total),
  EnetRcDecVars1 = EnetRcDecVars#enet_rc_dec_vars{decodeRange = DR1},
  Code1 = (EnetRcDecVars1#enet_rc_dec_vars.decodeCode - EnetRcDecVars1#enet_rc_dec_vars.decodeLow) div (DR1),
  {EnetRcDecVars1, Code1}.


enet_context_encode(EnetRcDecVars, EnetRcDecLoop1Vars, EnetRcDecVarsIdx, Update, Minimum) ->
  EnetRcDecLoop1Vars1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{under = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.value*Minimum, count = Minimum},

  EnetRcDecVarsSymbol = orddict:fetch(EnetRcDecVarsIdx, EnetRcDecVars#enet_rc_dec_vars.symbols),

  {EnetRcDecVars1, EnetRcDecLoop1Vars2} = if
    EnetRcDecVarsSymbol#enet_symbol.symbols == 0 ->
      EnetRcDecVars_1 = enet_symbol_create(EnetRcDecVars, EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.value, Update),
      EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars{symbol = EnetRcDecVars_1#enet_rc_dec_vars.currentSymbol},
      EnetRcDecVarsSymbol_1 = EnetRcDecVarsSymbol#enet_symbol{symbols = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.symbol - EnetRcDecVarsIdx},
      Symbols_1 = orddict:update(EnetRcDecVarsIdx, fun(_OldSymbol) -> EnetRcDecVarsSymbol_1 end, EnetRcDecVars_1#enet_rc_dec_vars.symbols),
      EnetRcDecVars_2 = EnetRcDecVars_1#enet_rc_dec_vars{symbols = Symbols_1},
      {EnetRcDecVars_2, EnetRcDecLoop1Vars_1};
    true ->
      EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars{encodeNode = EnetRcDecVarsIdx + EnetRcDecVarsSymbol#enet_symbol.symbols},
      enet_context_encode_loop1(EnetRcDecVars, EnetRcDecLoop1Vars_1, Update, Minimum)
  end,
  {EnetRcDecVars1, EnetRcDecLoop1Vars2}.


enet_context_encode_loop1(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum) ->
  NodeIdx = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.encodeNode,
  Node = orddict:fetch(NodeIdx, EnetRcDecVars#enet_rc_dec_vars.symbols),
  {EnetRcDecVars1, EnetRcDecLoop1Vars1} =
  if
    EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.value < Node#enet_symbol.value ->
      Node1 = Node#enet_symbol{under = Node#enet_symbol.under + Update},
      Symbols_1 = orddict:update(NodeIdx, fun(_OldSymbol) -> Node1 end, EnetRcDecVars#enet_rc_dec_vars.symbols),
      EnetRcDecVars_1 = EnetRcDecVars#enet_rc_dec_vars{symbols = Symbols_1},
      if
        Node1#enet_symbol.left /= 0 ->
          EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{encodeNode = NodeIdx + Node1#enet_symbol.left},
          enet_context_encode_loop1(EnetRcDecVars_1, EnetRcDecLoop1Vars_1, Update, Minimum);
        true ->
          EnetRcDecVars__1 = enet_symbol_create(EnetRcDecVars_1, EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.value, Update),
          EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{symbol = EnetRcDecVars__1#enet_rc_dec_vars.currentSymbol},
          Node_1 = Node1#enet_symbol{left = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.symbol - NodeIdx},
          Symbols__1 = orddict:update(NodeIdx, fun(_OldSymbol) -> Node_1 end, EnetRcDecVars__1#enet_rc_dec_vars.symbols),
          EnetRcDecVars__2 = EnetRcDecVars__1#enet_rc_dec_vars{symbols = Symbols__1},
          {EnetRcDecVars__2, EnetRcDecLoop1Vars_1}
      end;
    true ->
      if
        EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.value > Node#enet_symbol.value ->
          EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{under = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.under + Node#enet_symbol.under},
          if
            Node#enet_symbol.right /= 0 ->
              EnetRcDecLoop1Vars__1 = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars{encodeNode = NodeIdx + Node#enet_symbol.right},
              enet_context_encode_loop1(EnetRcDecVars, EnetRcDecLoop1Vars__1, Update, Minimum);
            true ->
              EnetRcDecVars__1 = enet_symbol_create(EnetRcDecVars, EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars.value, Update),
              EnetRcDecLoop1Vars__1 = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars{symbol = EnetRcDecVars__1#enet_rc_dec_vars.currentSymbol},
              Node_1 = Node#enet_symbol{right = EnetRcDecLoop1Vars__1#enet_rc_dec_loop1_vars.symbol - NodeIdx},
              Symbols__1 = orddict:update(NodeIdx, fun(_OldSymbol) -> Node_1 end, EnetRcDecVars__1#enet_rc_dec_vars.symbols),
              EnetRcDecVars__2 = EnetRcDecVars__1#enet_rc_dec_vars{symbols = Symbols__1},
              {EnetRcDecVars__2, EnetRcDecLoop1Vars__1}
          end;
        true ->
          Count1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.count + Node#enet_symbol.count,
          Under1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.under + (Node#enet_symbol.under - Node#enet_symbol.count),
          EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{count = Count1, under = Under1, symbol = NodeIdx},

          Node_1 = Node#enet_symbol{under = Node#enet_symbol.under + Update, count = Node#enet_symbol.count + Update},
          Symbols__1 = orddict:update(NodeIdx, fun(_OldSymbol) -> Node_1 end, EnetRcDecVars#enet_rc_dec_vars.symbols),
          EnetRcDecVars_1 = EnetRcDecVars#enet_rc_dec_vars{symbols = Symbols__1},
          {EnetRcDecVars_1, EnetRcDecLoop1Vars_1}
      end
  end,
  {EnetRcDecVars1, EnetRcDecLoop1Vars1}.


enet_context_decode(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum, IsSubcontext, CreateRoot, CreateRight, CreateLeft) ->
  EnetRcDecLoop1Vars1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{under = 0, count = Minimum},

  EnetRcDecVarsIdx = case IsSubcontext of
    false ->
      0;
    _ ->
      EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.subcontext
  end,
  Symbol = orddict:fetch(EnetRcDecVarsIdx, EnetRcDecVars#enet_rc_dec_vars.symbols),

  {EnetRcDecVars1, EnetRcDecLoop1Vars2} = if
    Symbol#enet_symbol.symbols == 0 ->
      case CreateRoot of
        true ->
          enet_create_root(EnetRcDecVars, EnetRcDecLoop1Vars1, Update, Minimum, IsSubcontext);
        _ ->
          dont_Have_CreateRoot
      end;
    true ->
      EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars{decodeNode = EnetRcDecVarsIdx + Symbol#enet_symbol.symbols},
      enet_context_decode_loop1(EnetRcDecVars, EnetRcDecLoop1Vars_1, Update, Minimum, CreateRight, CreateLeft)
  end,
  {EnetRcDecVars1, EnetRcDecLoop1Vars2}.


enet_context_decode_loop1(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum, CreateRight, CreateLeft) ->
  Node = orddict:fetch(EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.decodeNode, EnetRcDecVars#enet_rc_dec_vars.symbols),
  After = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.under + Node#enet_symbol.under + (Node#enet_symbol.value + 1)*Minimum,
  Before = Node#enet_symbol.count + Minimum,

  {Continue, EnetRcDecVars1, EnetRcDecLoop1Vars1} = if
    EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.code >= After ->
      Under1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.under + Node#enet_symbol.under,
      EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{under = Under1},
      if
        Node#enet_symbol.right /= 0 ->
          DecodeNode1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.decodeNode + Node#enet_symbol.right,
          EnetRcDecLoop1Vars__1 = EnetRcDecLoop1Vars_1#enet_rc_dec_loop1_vars{decodeNode = DecodeNode1},
          {true, EnetRcDecVars, EnetRcDecLoop1Vars__1};
        true ->
          case CreateRight of
            true ->
              {EnetRcDecVars__1, EnetRcDecLoop1Vars__1} = enet_create_right(EnetRcDecVars, EnetRcDecLoop1Vars_1, Update, Minimum, After),
              {false, EnetRcDecVars__1, EnetRcDecLoop1Vars__1};
            _ ->
              dont_Have_CreateRight
          end
      end;
    true ->
      if
        EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.code < (After - Before) ->
          NodeUnder1 = Node#enet_symbol.under + Update,
          Node_1 = Node#enet_symbol{under = NodeUnder1},
          Symbols1 = orddict:update(EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.decodeNode, fun(_OldSymbol) -> Node_1 end,
            EnetRcDecVars#enet_rc_dec_vars.symbols),
          EnetRcDecVars_1 = EnetRcDecVars#enet_rc_dec_vars{symbols = Symbols1},
          if
            Node_1#enet_symbol.left /= 0 ->
              DecodeNode1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.decodeNode + Node#enet_symbol.left,
              EnetRcDecLoop1Vars_1 =  EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{decodeNode = DecodeNode1},
              {true, EnetRcDecVars_1, EnetRcDecLoop1Vars_1};
            true ->
              case CreateLeft of
                true ->
                  {EnetRcDecVars__1, EnetRcDecLoop1Vars__1} = enet_create_left(EnetRcDecVars_1, EnetRcDecLoop1Vars, Update, Minimum, After, Before),
                  {false, EnetRcDecVars__1, EnetRcDecLoop1Vars__1};
                _ ->
                  dont_Have_CreateRight
              end
          end;
        true ->
          Value1 = Node#enet_symbol.value,
          Count1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.count + Node#enet_symbol.count,
          Under1 = After - Before,
          Symbol1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.decodeNode,
          EnetRcDecLoop1Vars_1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{value = Value1, count = Count1, under = Under1, symbol = Symbol1},

          Node_1 = Node#enet_symbol{under = Node#enet_symbol.under + Update, count = Node#enet_symbol.count + Update},
          Symbols2 = orddict:update(EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.decodeNode, fun(_OldSymbol) -> Node_1 end,
            EnetRcDecVars#enet_rc_dec_vars.symbols),
          EnetRcDecVars_1 = EnetRcDecVars#enet_rc_dec_vars{symbols = Symbols2},
          {false, EnetRcDecVars_1, EnetRcDecLoop1Vars_1}
      end
  end,
  if
    Continue == true ->
      enet_context_decode_loop1(EnetRcDecVars1, EnetRcDecLoop1Vars1, Update, Minimum, CreateRight, CreateLeft);
    true ->
      {EnetRcDecVars1, EnetRcDecLoop1Vars1}
  end
  .


enet_context_try_decode(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum) ->
  enet_context_decode(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum, true, false, false, false).


enet_context_root_decode(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum) ->
  enet_context_decode(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum, false, true, true, true).


enet_create_root(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum, IsSubcontext) ->
  Code1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.code,
  Value1 = Code1 div Minimum,
  Under1 = Code1 - Code1 rem Minimum,
  EnetRcDecLoop1Vars2 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{value = Value1, under = Under1},
  EnetRcDecVars1 = enet_symbol_create(EnetRcDecVars, EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars.value, Update),
  EnetRcDecLoop1Vars3 = EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars{symbol = EnetRcDecVars1#enet_rc_dec_vars.currentSymbol},
  Symbols1 = case IsSubcontext of
    false ->
      orddict:update(0, fun(OldSymbol) ->
        OldSymbol#enet_symbol{
          symbols = EnetRcDecLoop1Vars3#enet_rc_dec_loop1_vars.symbol
        }
      end, EnetRcDecVars1#enet_rc_dec_vars.symbols);
    _ ->
      orddict:update(EnetRcDecLoop1Vars3#enet_rc_dec_loop1_vars.subcontext, fun(OldSymbol) ->
        OldSymbol#enet_symbol{
          symbols = EnetRcDecLoop1Vars3#enet_rc_dec_loop1_vars.symbol - EnetRcDecLoop1Vars3#enet_rc_dec_loop1_vars.subcontext
        }
      end, EnetRcDecVars1#enet_rc_dec_vars.symbols)
  end,
  EnetRcDecVars2 = EnetRcDecVars1#enet_rc_dec_vars{symbols = Symbols1},

  {EnetRcDecVars2, EnetRcDecLoop1Vars3}.


enet_create_right(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum, After) ->
  Node = orddict:fetch(EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.decodeNode, EnetRcDecVars#enet_rc_dec_vars.symbols),
  Code1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.code,
  Value1 = Node#enet_symbol.value + 1 + (Code1 - After) div Minimum,
  Under1 = Code1 - (Code1 - After) rem Minimum,
  EnetRcDecLoop1Vars1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{value = Value1, under = Under1},
  EnetRcDecVars1 = enet_symbol_create(EnetRcDecVars, EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.value, Update),
  EnetRcDecLoop1Vars2 = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars{symbol = EnetRcDecVars1#enet_rc_dec_vars.currentSymbol},
  Node1 = Node#enet_symbol{right = EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars.symbol - EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars.decodeNode},
  Symbols1 = orddict:update(EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars.decodeNode, fun(_OldSymbol) -> Node1 end,
    EnetRcDecVars1#enet_rc_dec_vars.symbols),
  EnetRcDecVars2 = EnetRcDecVars1#enet_rc_dec_vars{symbols = Symbols1},
  {EnetRcDecVars2, EnetRcDecLoop1Vars2}.


enet_create_left(EnetRcDecVars, EnetRcDecLoop1Vars, Update, Minimum, After, Before) ->
  Node = orddict:fetch(EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.decodeNode, EnetRcDecVars#enet_rc_dec_vars.symbols),
  Code1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars.code,
  Value1 = Node#enet_symbol.value - 1 - (After - Before - Code1 - 1) div Minimum,
  Under1 = Code1 - (After - Before - Code1 - 1) rem Minimum,
  EnetRcDecLoop1Vars1 = EnetRcDecLoop1Vars#enet_rc_dec_loop1_vars{value = Value1, under = Under1},
  EnetRcDecVars1 = enet_symbol_create(EnetRcDecVars, EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars.value, Update),
  EnetRcDecLoop1Vars2 = EnetRcDecLoop1Vars1#enet_rc_dec_loop1_vars{symbol = EnetRcDecVars1#enet_rc_dec_vars.currentSymbol},
  Node1 = Node#enet_symbol{left = EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars.symbol - EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars.decodeNode},
  Symbols1 = orddict:update(EnetRcDecLoop1Vars2#enet_rc_dec_loop1_vars.decodeNode, fun(_OldSymbol) -> Node1 end,
    EnetRcDecVars1#enet_rc_dec_vars.symbols),
  EnetRcDecVars2 = EnetRcDecVars1#enet_rc_dec_vars{symbols = Symbols1},
  {EnetRcDecVars2, EnetRcDecLoop1Vars2}.


enet_symbol_create(EnetRcDecVars, Value, Count) ->
  CurrentSymbol = EnetRcDecVars#enet_rc_dec_vars.nextSymbol,
  Symbols_1 = orddict:update(CurrentSymbol, fun(_OldSymbol) ->
    #enet_symbol{
        value = Value,
        count = Count,
        under = Count,
        left = 0,
        right = 0,
        symbols = 0,
        escapes = 0,
        total = 0,
        parent = 0
      }
  end, EnetRcDecVars#enet_rc_dec_vars.symbols),
  EnetRcDecVars#enet_rc_dec_vars{symbols = Symbols_1, currentSymbol = CurrentSymbol, nextSymbol = CurrentSymbol+1}.


enet_context_create(EnetRcDecVars, Escapes, Minimum) ->
  EnetRcDecVars1 = enet_symbol_create(EnetRcDecVars, 0, 0),
  CurrentSymbol = EnetRcDecVars1#enet_rc_dec_vars.currentSymbol,
  NewSymbols = orddict:update(CurrentSymbol, fun(OldSymbol) ->
    OldSymbol#enet_symbol{
      escapes = Escapes,
      total = Escapes + 256 * Minimum,
      symbols = 0
      }
  end, EnetRcDecVars1#enet_rc_dec_vars.symbols),
  EnetRcDecVars1#enet_rc_dec_vars{symbols = NewSymbols}.


enet_range_coder_seed(EnetRcDecVars, Payload) ->
  D1 = EnetRcDecVars#enet_rc_dec_vars.decodeCode,
  P1 = Payload,
  {D2, P2} = case byte_size(P1) of
    0 ->
      {D1, P1};
    _ ->
      <<Val1:8/unsigned-integer, Rest1/binary>> = P1,
      {D1 bor (Val1 bsl 24), Rest1}
    end,
  {D3, P3} = case byte_size(P2) of
    0 ->
      {D2, P2};
    _ ->
      <<Val2:8/unsigned-integer, Rest2/binary>> = P2,
      {D2 bor (Val2 bsl 16), Rest2}
    end,
  {D4, P4} = case byte_size(P3) of
    0 ->
      {D3, P3};
    _ ->
      <<Val3:8/unsigned-integer, Rest3/binary>> = P3,
      {D3 bor (Val3 bsl 8), Rest3}
    end,
  {D5, P5} = case byte_size(P4) of
    0 ->
      {D4, P4};
    _ ->
      <<Val4:8/unsigned-integer, Rest4/binary>> = P4,
      {D4 bor (Val4), Rest4}
    end,
  {EnetRcDecVars#enet_rc_dec_vars{decodeCode = D5}, P5}.


enet_range_coder_destroy(_Arg) -> ok.


to_uint32(Val) ->
  to_uint(Val, 32).

to_uint(Val, Size) ->
  ValBin = binary:encode_unsigned(Val),
  if
    byte_size(ValBin) > (Size div 8) ->
      X = byte_size(ValBin) - (Size div 8),
      <<_:X/binary, Retval:Size/unsigned-integer>> = ValBin,
      Retval;
    true ->
      Val
  end
  .

