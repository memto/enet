-module(enet_compress).

-include("enet_peer.hrl").
-include("enet_commands.hrl").
-include("enet_protocol.hrl").
-include("enet_compress.hrl").

-export([
         enet_get_compressor/1,
         enet_range_coder_compress/1,
         enet_range_coder_decompress/1,
         enet_range_coder_destroy/1
        ]).


enet_get_compressor(range_coder) ->
  #enet_compressor {
    context = enet_range_coder_create(),
    compress = {enet_compress, enet_range_coder_compress, []},
    decompress = {enet_compress, enet_range_coder_decompress, []},
    destroy = {enet_compress, enet_range_coder_destroy, []}
  }.


enet_range_coder_create() ->
  orddict:from_list([{Idx, #enet_symbol{}} || Idx <- lists:seq(0,4095)]).
  % orddict:from_list([{Idx, #enet_symbol{}} || Idx <- lists:seq(0,4)]).


enet_range_coder_compress(_Payload) ->
  io:fwrite("enet_range_coder_compress ~n").


enet_range_coder_decompress(Payload) ->
  Symbols = orddict:from_list([{Idx, #enet_symbol{}} || Idx <- lists:seq(0,4095)]),

  Context = #enet_range_coder{
    symbols = Symbols,

    decodeLow = 0,
    decodeCode = 0,
    decodeRange = ?MAX_UINT32,

    root = 0,
    predicted = 0,
    order = 0,
    nextSymbol = 0
  },

  Context1 = enet_context_create(Context, ?ENET_CONTEXT_ESCAPE_MINIMUM, ?ENET_CONTEXT_SYMBOL_MINIMUM),
  {Context2, Payload1} = enet_range_coder_seed(Context1, Payload),
  Commands = do_enet_range_coder_decompress(Context2, Payload1),

  Commands.


do_enet_range_coder_decompress(Context, Payload) ->
  Loop1Ctx = #enet_rc_dec_loop1{
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
  enet_range_coder_decompress_loop1(Context, Loop1Ctx, Payload, <<>>).


enet_range_coder_decompress_loop1(Context, Loop1Ctx, Payload, Output) ->
  Loop1Ctx_x = Loop1Ctx#enet_rc_dec_loop1{subcontext = Context#enet_range_coder.predicted},
  {Ctx1, L1Ctx1, Payload1, Out1, GotoPatchContexts} =
    enet_range_coder_decompress_loop1_top(Context, Loop1Ctx_x, Payload, Output),

  {Ctx2, L1Ctx2, Payload2, Out2, Break} = case GotoPatchContexts of
    false ->
      enet_range_coder_decompress_loop1_middle(Ctx1, L1Ctx1, Payload1, Out1);
    _ ->
      {Ctx1, L1Ctx1, Payload1, Out1, false}
    end,

  Out3 = case Break of
    true ->
      Out2;
    _ ->
      Loop1Ctx_2 = L1Ctx2#enet_rc_dec_loop1{patch = Ctx2#enet_range_coder.predicted},
      {Ctx_1, _L1Ctx_1, Payload_1, Out_1} = enet_range_coder_decompress_loop1_bottom(Ctx2, Loop1Ctx_2, Payload2, Out2),
      NewLoop1Ctx = #enet_rc_dec_loop1{
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
      enet_range_coder_decompress_loop1(Ctx_1, NewLoop1Ctx, Payload_1, Out_1)
    end,
  Out3
  .


enet_range_coder_decompress_loop1_top(Context, Loop1Ctx, Payload, Output) ->
  SubcontextIdx = Loop1Ctx#enet_rc_dec_loop1.subcontext,
  {Context1, Loop1Ctx1, Payload1, Output1, GotoPatchContexts} =
  if
    SubcontextIdx == 0 ->
      {Context, Loop1Ctx, Payload, Output, false};
    true ->
      SubContextSymbol = orddict:fetch(SubcontextIdx, Context#enet_range_coder.symbols),
      if
        SubContextSymbol#enet_symbol.escapes =< 0 ->
          Loop1Ctx_1 = Loop1Ctx#enet_rc_dec_loop1{subcontext = SubContextSymbol#enet_symbol.parent},
          enet_range_coder_decompress_loop1_top(Context, Loop1Ctx_1, Payload, Output);
        true ->
          Loop1Ctx_1 = Loop1Ctx#enet_rc_dec_loop1{total = SubContextSymbol#enet_symbol.total},
          if
            SubContextSymbol#enet_symbol.escapes >= Loop1Ctx_1#enet_rc_dec_loop1.total ->
              Loop1Ctx__1 = Loop1Ctx_1#enet_rc_dec_loop1{subcontext = SubContextSymbol#enet_symbol.parent},
              enet_range_coder_decompress_loop1_top(Context, Loop1Ctx__1, Payload, Output);
            true ->
              {Context__1, Code1}  = enet_range_coder_read(Context, Loop1Ctx_1#enet_rc_dec_loop1.total),
              Loop1Ctx__1 = Loop1Ctx_1#enet_rc_dec_loop1{code = Code1},
              if
                Loop1Ctx__1#enet_rc_dec_loop1.code < SubContextSymbol#enet_symbol.escapes ->
                  {Context___1, Payload___1} = enet_range_coder_decode(Context__1, 0, SubContextSymbol#enet_symbol.escapes, Loop1Ctx__1#enet_rc_dec_loop1.total, Payload),
                  Loop1Ctx___1 = Loop1Ctx__1#enet_rc_dec_loop1{subcontext = SubContextSymbol#enet_symbol.parent},
                  enet_range_coder_decompress_loop1_top(Context___1, Loop1Ctx___1, Payload___1, Output);
                true ->
                  Code2 = Loop1Ctx__1#enet_rc_dec_loop1.code - SubContextSymbol#enet_symbol.escapes,
                  Loop1Ctx___1 = Loop1Ctx__1#enet_rc_dec_loop1{code = Code2},

                  {Context___1, Loop1Ctx___2} = enet_context_try_decode(Context__1, Loop1Ctx___1, ?ENET_SUBCONTEXT_SYMBOL_DELTA, 0),


                  Loop1Ctx___3 = Loop1Ctx___2#enet_rc_dec_loop1{bottom = Loop1Ctx___2#enet_rc_dec_loop1.symbol},

                  {Context___2, Payload___1} = enet_range_coder_decode(Context___1,
                    SubContextSymbol#enet_symbol.escapes + Loop1Ctx___3#enet_rc_dec_loop1.under,
                    Loop1Ctx___3#enet_rc_dec_loop1.count, Loop1Ctx___3#enet_rc_dec_loop1.total,
                    Payload),
                  Symbols1 = orddict:update(SubcontextIdx, fun(OldSymbol) ->
                    OldSymbol#enet_symbol{
                      total = OldSymbol#enet_symbol.total + ?ENET_SUBCONTEXT_SYMBOL_DELTA
                    }
                  end, Context___2#enet_range_coder.symbols),
                  Context___3 = Context___2#enet_range_coder{symbols = Symbols1},

                  SubContextSymbol_1 = orddict:fetch(SubcontextIdx, Context___3#enet_range_coder.symbols),
                  XXX = Loop1Ctx___3#enet_rc_dec_loop1.count > (255 - 2 * ?ENET_SUBCONTEXT_SYMBOL_DELTA),
                  YYY = SubContextSymbol_1#enet_symbol.total > ?ENET_RANGE_CODER_BOTTOM - 256,
                  if
                    XXX or YYY ->
                      Context___4 = enet_context_rescale(Context___3, SubcontextIdx, 0),
                      {Context___4, Loop1Ctx___3, Payload___1, Output, true};
                    true ->
                      {Context___3, Loop1Ctx___3, Payload___1, Output, true}
                  end
              end
          end
      end
  end,
  {Context1, Loop1Ctx1, Payload1, Output1, GotoPatchContexts}.


enet_range_coder_decompress_loop1_middle(Context, Loop1Ctx, Payload, Output) ->
  Root1 = orddict:fetch(0, Context#enet_range_coder.symbols),
  Loop1Ctx1_x = Loop1Ctx#enet_rc_dec_loop1{total = Root1#enet_symbol.total},

  Root2 = orddict:fetch(0, Context#enet_range_coder.symbols),


  {Context1, Code1} = enet_range_coder_read(Context, Loop1Ctx1_x#enet_rc_dec_loop1.total),
  Loop1Ctx1 = Loop1Ctx1_x#enet_rc_dec_loop1{code = Code1},


  {Context2, Payload1, Break} = if
    Code1 < Root2#enet_symbol.escapes ->
      {Context_1, Payload_1} = enet_range_coder_decode(Context1, 0, Root2#enet_symbol.escapes, Loop1Ctx1#enet_rc_dec_loop1.total, Payload),
      {Context_1, Payload_1, true};
    true ->
      {Context1, Payload, false}
  end,

  {Context3, Loop1Ctx2, Payload2, Output1, Break2} = if
    Break == true ->
      {Context2, Loop1Ctx1, Payload1, Output, Break};
    true ->
      Root3 = orddict:fetch(0, Context2#enet_range_coder.symbols),
      Loop1Ctx_1 = Loop1Ctx1#enet_rc_dec_loop1{code = Loop1Ctx1#enet_rc_dec_loop1.code - Root3#enet_symbol.escapes},
      {Context__1, Loop1Ctx_2} = enet_context_root_decode(Context2, Loop1Ctx_1, ?ENET_CONTEXT_SYMBOL_DELTA, ?ENET_CONTEXT_SYMBOL_MINIMUM),
      Loop1Ctx_3 = Loop1Ctx_2#enet_rc_dec_loop1{bottom = Loop1Ctx_2#enet_rc_dec_loop1.symbol},
      Root4 = orddict:fetch(0, Context__1#enet_range_coder.symbols),
      {Context_2, Payload__1} = enet_range_coder_decode(Context__1,
        Root4#enet_symbol.escapes + Loop1Ctx_3#enet_rc_dec_loop1.under,
        Loop1Ctx_3#enet_rc_dec_loop1.count, Loop1Ctx_3#enet_rc_dec_loop1.total,
        Payload1),


      Symbols1 = orddict:update(0, fun(OldSymbol) ->
        OldSymbol#enet_symbol{total = OldSymbol#enet_symbol.total + ?ENET_CONTEXT_SYMBOL_DELTA}
      end, Context_2#enet_range_coder.symbols),
      Context_3 = Context_2#enet_range_coder{symbols = Symbols1},
      Root5 = orddict:fetch(0, Context_3#enet_range_coder.symbols),
      XXX = Loop1Ctx_3#enet_rc_dec_loop1.count > (255 - 2 * ?ENET_CONTEXT_SYMBOL_DELTA + ?ENET_CONTEXT_SYMBOL_MINIMUM),
      YYY = Root5#enet_symbol.total > ?ENET_RANGE_CODER_BOTTOM - 256,
      if
        XXX or YYY ->
          Context_4 = enet_context_rescale(Context_3, 0, ?ENET_CONTEXT_SYMBOL_MINIMUM),
          {Context_4, Loop1Ctx_3, Payload__1, Output, Break};
        true ->
          {Context_3, Loop1Ctx_3, Payload__1, Output, Break}
      end
  end,
  {Context3, Loop1Ctx2, Payload2, Output1, Break2}
  .


enet_range_coder_decompress_loop1_bottom(Context, Loop1Ctx, Payload, Output) ->
  {Context1, Loop1Ctx1, Payload1, Output1} = enet_range_coder_decompress_loop1_bottom_loop1(Context, Loop1Ctx, Payload, Output),
  Context2 = if
    Loop1Ctx1#enet_rc_dec_loop1.parent == -1 ->
      Context1#enet_range_coder{predicted = Loop1Ctx1#enet_rc_dec_loop1.bottom};
    true ->
      Symbols_1 = orddict:update(Loop1Ctx1#enet_rc_dec_loop1.parent, fun(OldSymbol) ->
        OldSymbol#enet_symbol{
          parent = Loop1Ctx1#enet_rc_dec_loop1.bottom
        }
      end, Context1#enet_range_coder.symbols),
      Context1#enet_range_coder{symbols = Symbols_1}
  end,
  Val = Loop1Ctx1#enet_rc_dec_loop1.value,
  ValBin = binary:encode_unsigned(Val),
  Output2 = <<Output1/binary, ValBin/binary>>,
  Context3 = if
    Context2#enet_range_coder.order >= ?ENET_SUBCONTEXT_ORDER ->
      Symbol_1 = orddict:fetch(Context2#enet_range_coder.predicted, Context2#enet_range_coder.symbols),
      Context2#enet_range_coder{predicted = Symbol_1#enet_symbol.parent};
    true ->
      Context2#enet_range_coder{order = Context2#enet_range_coder.order + 1}
  end,
  Context4 = enet_range_coder_free_symbols(Context3),
  {Context4, Loop1Ctx1, Payload1, Output2}
  .


enet_range_coder_free_symbols(Context) ->
  if
    Context#enet_range_coder.nextSymbol >= 4096 - ?ENET_SUBCONTEXT_ORDER ->
      Context_1 = Context#enet_range_coder{nextSymbol = 0},
      Context_2 = enet_context_create(Context_1, ?ENET_CONTEXT_ESCAPE_MINIMUM, ?ENET_CONTEXT_SYMBOL_MINIMUM),
      Context_2#enet_range_coder{predicted = 0, order = 0};
    true ->
      Context
  end
  .


enet_range_coder_decompress_loop1_bottom_loop1(Context, Loop1Ctx, Payload, Output) ->
  PatchIdx = Loop1Ctx#enet_rc_dec_loop1.patch,
  SubcontextIdx = Loop1Ctx#enet_rc_dec_loop1.subcontext,
  {Context1, Loop1Ctx1, Payload1, Output1} = if
      SubcontextIdx == PatchIdx ->
        {Context, Loop1Ctx, Payload, Output};
      true ->
        {Context_1, Loop1Ctx_1} = enet_context_encode(Context, Loop1Ctx, PatchIdx, ?ENET_SUBCONTEXT_SYMBOL_DELTA, 0),
        Context_2 = if
          Loop1Ctx_1#enet_rc_dec_loop1.parent == -1 ->
            Context_1#enet_range_coder{predicted = Loop1Ctx_1#enet_rc_dec_loop1.symbol};
          true ->
            Symbols_1 = orddict:update(Loop1Ctx_1#enet_rc_dec_loop1.parent, fun(OldSymbol) ->
              OldSymbol#enet_symbol{
                parent = Loop1Ctx_1#enet_rc_dec_loop1.symbol
              }
            end, Context_1#enet_range_coder.symbols),
            Context_1#enet_range_coder{symbols = Symbols_1}
        end,
        Loop1Ctx__1 = Loop1Ctx_1#enet_rc_dec_loop1{parent = Loop1Ctx_1#enet_rc_dec_loop1.symbol},
        PatchSymbol = orddict:fetch(PatchIdx, Context_2#enet_range_coder.symbols),
        PatchSymbol_1 = if
          Loop1Ctx__1#enet_rc_dec_loop1.count =< 0 ->
            PatchSymbol#enet_symbol{
              escapes = PatchSymbol#enet_symbol.escapes + ?ENET_SUBCONTEXT_ESCAPE_DELTA,
              total = PatchSymbol#enet_symbol.total + ?ENET_SUBCONTEXT_ESCAPE_DELTA
            };
          true ->
            PatchSymbol
        end,
        PatchSymbol_2 = PatchSymbol_1#enet_symbol{total = PatchSymbol_1#enet_symbol.total + ?ENET_SUBCONTEXT_SYMBOL_DELTA},
        Symbols_2 = orddict:update(PatchIdx, fun(_OldSymbol) -> PatchSymbol_2 end, Context_2#enet_range_coder.symbols),
        Context_3 = Context_2#enet_range_coder{symbols = Symbols_2},

        XXX = Loop1Ctx__1#enet_rc_dec_loop1.count > (255 - 2 * ?ENET_SUBCONTEXT_SYMBOL_DELTA),
        YYY = PatchSymbol_2#enet_symbol.total > ?ENET_RANGE_CODER_BOTTOM - 256,
        Context_4 = if
          XXX or YYY ->
            enet_context_rescale(Context_3, PatchIdx, 0);
          true ->
            Context_3
        end,

        PatchSymbol_3 = orddict:fetch(PatchIdx, Context_4#enet_range_coder.symbols),
        Loop1Ctx__2 = Loop1Ctx__1#enet_rc_dec_loop1{patch = PatchSymbol_3#enet_symbol.parent},
        enet_range_coder_decompress_loop1_bottom_loop1(Context_4, Loop1Ctx__2, Payload, Output)
    end,
  {Context1, Loop1Ctx1, Payload1, Output1}
  .


enet_range_coder_decode(Context, Under, Count, _Total, Payload) ->
  DecodeLow1 = to_uint32(Context#enet_range_coder.decodeLow + (Under * Context#enet_range_coder.decodeRange)),
  DecodeRange1 = to_uint32(Context#enet_range_coder.decodeRange * Count),
  Context1 = Context#enet_range_coder{decodeLow = DecodeLow1, decodeRange = DecodeRange1},
  {Context2, Payload1} = enet_range_coder_decode_loop1(Context1, Payload),
  {Context2, Payload1}.


enet_range_coder_decode_loop1(Context, Payload) ->
  #enet_range_coder{
    decodeLow = DecodeLow1,
    decodeRange = DecodeRange1,
    decodeCode = _DecodeCode1
  } = Context,

  {Context1, Break} = if
    (DecodeLow1 bxor (DecodeLow1 + DecodeRange1)) >= ?ENET_RANGE_CODER_TOP ->
      if
        DecodeRange1 >= ?ENET_RANGE_CODER_BOTTOM ->
          {Context, true};
        true ->
          DecodeRange3 = to_uint32(-DecodeLow1 band (?ENET_RANGE_CODER_BOTTOM - 1)),
          {Context#enet_range_coder{decodeRange = DecodeRange3}, false}
      end;
    true ->
      {Context, false}
  end,

  {Context2, Payload1} =
  case Break of
    false ->
        #enet_range_coder{
          decodeLow = DecodeLow_2,
          decodeRange = DecodeRange_2,
          decodeCode = DecodeCode_2
        } = Context1,

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
      Context_1 = Context1#enet_range_coder{decodeLow = DecodeLow_3, decodeRange=DecodeRange_3, decodeCode = DecodeCode_4},
      enet_range_coder_decode_loop1(Context_1, Payload_1);
    _ ->
      {Context1, Payload}
  end,
  {Context2, Payload1}.


enet_symbol_rescale(Context, SymbolIdx, Total) ->
  Symbol = orddict:fetch(SymbolIdx, Context#enet_range_coder.symbols),
  Count1 = Symbol#enet_symbol.count - (Symbol#enet_symbol.count bsr 1),
  Symbol1 = Symbol#enet_symbol{count = Count1, under = Count1},
  {Context1, Under1} = if
    Symbol1#enet_symbol.left /= 0 ->
      Symbols1 = orddict:update(SymbolIdx, fun(_OldSymbol) -> Symbol1 end, Context#enet_range_coder.symbols),
      Context_1 = Context#enet_range_coder{symbols = Symbols1},
      enet_symbol_rescale(Context_1, SymbolIdx + Symbol1#enet_symbol.left, Total);
    true ->
      {Context, 0}
  end,
  Total1 = Total + Under1,
  Symbol2 = Symbol1#enet_symbol{under = Symbol1#enet_symbol.under + Under1},
  {Context2, Total2} = if
    Symbol2#enet_symbol.right == 0 ->
      {Context1, Total1};
    true ->
      enet_symbol_rescale(Context1, SymbolIdx + Symbol2#enet_symbol.right, Total1)
  end,
  {Context2, Total2}.


enet_context_rescale(Context, SymbolIdx, Minimum) ->
  Symbol = orddict:fetch(SymbolIdx, Context#enet_range_coder.symbols),

  {Context1, Total} = if
    Symbol#enet_symbol.symbols /= 0 ->
      enet_symbol_rescale(Context, SymbolIdx + Symbol#enet_symbol.symbols, 0);
    true ->
      {Context, 0}
  end,
  Symbol1 = Symbol#enet_symbol{total = Total},
  Escapes = Symbol1#enet_symbol.escapes - (Symbol1#enet_symbol.escapes bsr 1),
  Symbol2 = Symbol1#enet_symbol{escapes = Escapes},
  Total2 = Symbol2#enet_symbol.total + Symbol2#enet_symbol.escapes + 256 * Minimum,
  Symbol3 = Symbol2#enet_symbol{total = Total2},
  Symbols1 = orddict:update(SymbolIdx, fun(_OldSymbol) -> Symbol3 end, Context1#enet_range_coder.symbols),
  Context1#enet_range_coder{symbols = Symbols1}.


enet_range_coder_read(Context, Total) ->
  DR1 = to_uint32(Context#enet_range_coder.decodeRange div Total),
  Ctx1 = Context#enet_range_coder{decodeRange = DR1},
  Code1 = (Ctx1#enet_range_coder.decodeCode - Ctx1#enet_range_coder.decodeLow) div (DR1),
  {Ctx1, Code1}.


enet_context_encode(Context, Loop1Ctx, ContextIdx, Update, Minimum) ->
  Loop1Ctx1 = Loop1Ctx#enet_rc_dec_loop1{under = Loop1Ctx#enet_rc_dec_loop1.value*Minimum, count = Minimum},

  ContextSymbol = orddict:fetch(ContextIdx, Context#enet_range_coder.symbols),

  {Context1, Loop1Ctx2} = if
    ContextSymbol#enet_symbol.symbols == 0 ->
      Context_1 = enet_symbol_create(Context, Loop1Ctx1#enet_rc_dec_loop1.value, Update),
      Loop1Ctx_1 = Loop1Ctx1#enet_rc_dec_loop1{symbol = Context_1#enet_range_coder.currentSymbol},
      ContextSymbol_1 = ContextSymbol#enet_symbol{symbols = Loop1Ctx_1#enet_rc_dec_loop1.symbol - ContextIdx},
      Symbols_1 = orddict:update(ContextIdx, fun(_OldSymbol) -> ContextSymbol_1 end, Context_1#enet_range_coder.symbols),
      Context_2 = Context_1#enet_range_coder{symbols = Symbols_1},
      {Context_2, Loop1Ctx_1};
    true ->
      Loop1Ctx_1 = Loop1Ctx1#enet_rc_dec_loop1{encodeNode = ContextIdx + ContextSymbol#enet_symbol.symbols},
      enet_context_encode_loop1(Context, Loop1Ctx_1, Update, Minimum)
  end,
  {Context1, Loop1Ctx2}.


enet_context_encode_loop1(Context, Loop1Ctx, Update, Minimum) ->
  NodeIdx = Loop1Ctx#enet_rc_dec_loop1.encodeNode,
  Node = orddict:fetch(NodeIdx, Context#enet_range_coder.symbols),
  {Context1, Loop1Ctx1} =
  if
    Loop1Ctx#enet_rc_dec_loop1.value < Node#enet_symbol.value ->
      Node1 = Node#enet_symbol{under = Node#enet_symbol.under + Update},
      Symbols_1 = orddict:update(NodeIdx, fun(_OldSymbol) -> Node1 end, Context#enet_range_coder.symbols),
      Context_1 = Context#enet_range_coder{symbols = Symbols_1},
      if
        Node1#enet_symbol.left /= 0 ->
          Loop1Ctx_1 = Loop1Ctx#enet_rc_dec_loop1{encodeNode = NodeIdx + Node1#enet_symbol.left},
          enet_context_encode_loop1(Context_1, Loop1Ctx_1, Update, Minimum);
        true ->
          Context__1 = enet_symbol_create(Context_1, Loop1Ctx#enet_rc_dec_loop1.value, Update),
          Loop1Ctx_1 = Loop1Ctx#enet_rc_dec_loop1{symbol = Context__1#enet_range_coder.currentSymbol},
          Node_1 = Node1#enet_symbol{left = Loop1Ctx_1#enet_rc_dec_loop1.symbol - NodeIdx},
          Symbols__1 = orddict:update(NodeIdx, fun(_OldSymbol) -> Node_1 end, Context__1#enet_range_coder.symbols),
          Context__2 = Context__1#enet_range_coder{symbols = Symbols__1},
          {Context__2, Loop1Ctx_1}
      end;
    true ->
      if
        Loop1Ctx#enet_rc_dec_loop1.value > Node#enet_symbol.value ->
          Loop1Ctx_1 = Loop1Ctx#enet_rc_dec_loop1{under = Loop1Ctx#enet_rc_dec_loop1.under + Node#enet_symbol.under},
          if
            Node#enet_symbol.right /= 0 ->
              Loop1Ctx__1 = Loop1Ctx_1#enet_rc_dec_loop1{encodeNode = NodeIdx + Node#enet_symbol.right},
              enet_context_encode_loop1(Context, Loop1Ctx__1, Update, Minimum);
            true ->
              Context__1 = enet_symbol_create(Context, Loop1Ctx_1#enet_rc_dec_loop1.value, Update),
              Loop1Ctx__1 = Loop1Ctx_1#enet_rc_dec_loop1{symbol = Context__1#enet_range_coder.currentSymbol},
              Node_1 = Node#enet_symbol{right = Loop1Ctx__1#enet_rc_dec_loop1.symbol - NodeIdx},
              Symbols__1 = orddict:update(NodeIdx, fun(_OldSymbol) -> Node_1 end, Context__1#enet_range_coder.symbols),
              Context__2 = Context__1#enet_range_coder{symbols = Symbols__1},
              {Context__2, Loop1Ctx__1}
          end;
        true ->
          Count1 = Loop1Ctx#enet_rc_dec_loop1.count + Node#enet_symbol.count,
          Under1 = Loop1Ctx#enet_rc_dec_loop1.under + (Node#enet_symbol.under - Node#enet_symbol.count),
          Loop1Ctx_1 = Loop1Ctx#enet_rc_dec_loop1{count = Count1, under = Under1, symbol = NodeIdx},

          Node_1 = Node#enet_symbol{under = Node#enet_symbol.under + Update, count = Node#enet_symbol.count + Update},
          Symbols__1 = orddict:update(NodeIdx, fun(_OldSymbol) -> Node_1 end, Context#enet_range_coder.symbols),
          Context_1 = Context#enet_range_coder{symbols = Symbols__1},
          {Context_1, Loop1Ctx_1}
      end
  end,
  {Context1, Loop1Ctx1}.


enet_context_decode(Context, Loop1Ctx, Update, Minimum, IsSubcontext, CreateRoot, CreateRight, CreateLeft) ->
  Loop1Ctx1 = Loop1Ctx#enet_rc_dec_loop1{under = 0, count = Minimum},

  ContextIdx = case IsSubcontext of
    false ->
      0;
    _ ->
      Loop1Ctx1#enet_rc_dec_loop1.subcontext
  end,
  Symbol = orddict:fetch(ContextIdx, Context#enet_range_coder.symbols),

  {Context1, Loop1Ctx2} = if
    Symbol#enet_symbol.symbols == 0 ->
      case CreateRoot of
        true ->
          enet_create_root(Context, Loop1Ctx1, Update, Minimum, IsSubcontext);
        _ ->
          dont_Have_CreateRoot
      end;
    true ->
      Loop1Ctx_1 = Loop1Ctx1#enet_rc_dec_loop1{decodeNode = ContextIdx + Symbol#enet_symbol.symbols},
      enet_context_decode_loop1(Context, Loop1Ctx_1, Update, Minimum, CreateRight, CreateLeft)
  end,
  {Context1, Loop1Ctx2}.


enet_context_decode_loop1(Context, Loop1Ctx, Update, Minimum, CreateRight, CreateLeft) ->
  Node = orddict:fetch(Loop1Ctx#enet_rc_dec_loop1.decodeNode, Context#enet_range_coder.symbols),
  After = Loop1Ctx#enet_rc_dec_loop1.under + Node#enet_symbol.under + (Node#enet_symbol.value + 1)*Minimum,
  Before = Node#enet_symbol.count + Minimum,

  {Continue, Context1, Loop1Ctx1} = if
    Loop1Ctx#enet_rc_dec_loop1.code >= After ->
      Under1 = Loop1Ctx#enet_rc_dec_loop1.under + Node#enet_symbol.under,
      Loop1Ctx_1 = Loop1Ctx#enet_rc_dec_loop1{under = Under1},
      if
        Node#enet_symbol.right /= 0 ->
          DecodeNode1 = Loop1Ctx#enet_rc_dec_loop1.decodeNode + Node#enet_symbol.right,
          Loop1Ctx__1 = Loop1Ctx_1#enet_rc_dec_loop1{decodeNode = DecodeNode1},
          {true, Context, Loop1Ctx__1};
        true ->
          case CreateRight of
            true ->
              {Context__1, Loop1Ctx__1} = enet_create_right(Context, Loop1Ctx_1, Update, Minimum, After),
              {false, Context__1, Loop1Ctx__1};
            _ ->
              dont_Have_CreateRight
          end
      end;
    true ->
      if
        Loop1Ctx#enet_rc_dec_loop1.code < (After - Before) ->
          NodeUnder1 = Node#enet_symbol.under + Update,
          Node_1 = Node#enet_symbol{under = NodeUnder1},
          Symbols1 = orddict:update(Loop1Ctx#enet_rc_dec_loop1.decodeNode, fun(_OldSymbol) -> Node_1 end,
            Context#enet_range_coder.symbols),
          Context_1 = Context#enet_range_coder{symbols = Symbols1},
          if
            Node_1#enet_symbol.left /= 0 ->
              DecodeNode1 = Loop1Ctx#enet_rc_dec_loop1.decodeNode + Node#enet_symbol.left,
              Loop1Ctx_1 =  Loop1Ctx#enet_rc_dec_loop1{decodeNode = DecodeNode1},
              {true, Context_1, Loop1Ctx_1};
            true ->
              case CreateLeft of
                true ->
                  {Context__1, Loop1Ctx__1} = enet_create_left(Context_1, Loop1Ctx, Update, Minimum, After, Before),
                  {false, Context__1, Loop1Ctx__1};
                _ ->
                  dont_Have_CreateRight
              end
          end;
        true ->
          Value1 = Node#enet_symbol.value,
          Count1 = Loop1Ctx#enet_rc_dec_loop1.count + Node#enet_symbol.count,
          Under1 = After - Before,
          Symbol1 = Loop1Ctx#enet_rc_dec_loop1.decodeNode,
          Loop1Ctx_1 = Loop1Ctx#enet_rc_dec_loop1{value = Value1, count = Count1, under = Under1, symbol = Symbol1},

          Node_1 = Node#enet_symbol{under = Node#enet_symbol.under + Update, count = Node#enet_symbol.count + Update},
          Symbols2 = orddict:update(Loop1Ctx#enet_rc_dec_loop1.decodeNode, fun(_OldSymbol) -> Node_1 end,
            Context#enet_range_coder.symbols),
          Context_1 = Context#enet_range_coder{symbols = Symbols2},
          {false, Context_1, Loop1Ctx_1}
      end
  end,
  if
    Continue == true ->
      enet_context_decode_loop1(Context1, Loop1Ctx1, Update, Minimum, CreateRight, CreateLeft);
    true ->
      {Context1, Loop1Ctx1}
  end
  .


enet_context_try_decode(Context, Loop1Ctx, Update, Minimum) ->
  enet_context_decode(Context, Loop1Ctx, Update, Minimum, true, false, false, false).


enet_context_root_decode(Context, Loop1Ctx, Update, Minimum) ->
  enet_context_decode(Context, Loop1Ctx, Update, Minimum, false, true, true, true).


enet_create_root(Context, Loop1Ctx, Update, Minimum, IsSubcontext) ->
  Code1 = Loop1Ctx#enet_rc_dec_loop1.code,
  Value1 = Code1 div Minimum,
  Under1 = Code1 - Code1 rem Minimum,
  Loop1Ctx2 = Loop1Ctx#enet_rc_dec_loop1{value = Value1, under = Under1},
  Context1 = enet_symbol_create(Context, Loop1Ctx2#enet_rc_dec_loop1.value, Update),
  Loop1Ctx3 = Loop1Ctx2#enet_rc_dec_loop1{symbol = Context1#enet_range_coder.currentSymbol},
  Symbols1 = case IsSubcontext of
    false ->
      orddict:update(Context1#enet_range_coder.root, fun(OldSymbol) ->
        OldSymbol#enet_symbol{
          symbols = Loop1Ctx3#enet_rc_dec_loop1.symbol - Context1#enet_range_coder.root
        }
      end, Context1#enet_range_coder.symbols);
    _ ->
      orddict:update(Loop1Ctx3#enet_rc_dec_loop1.subcontext, fun(OldSymbol) ->
        OldSymbol#enet_symbol{
          symbols = Loop1Ctx3#enet_rc_dec_loop1.symbol - Loop1Ctx3#enet_rc_dec_loop1.subcontext
        }
      end, Context1#enet_range_coder.symbols)
  end,
  Context2 = Context1#enet_range_coder{symbols = Symbols1},

  {Context2, Loop1Ctx3}.


enet_create_right(Context, Loop1Ctx, Update, Minimum, After) ->
  Node = orddict:fetch(Loop1Ctx#enet_rc_dec_loop1.decodeNode, Context#enet_range_coder.symbols),
  Code1 = Loop1Ctx#enet_rc_dec_loop1.code,
  Value1 = Node#enet_symbol.value + 1 + (Code1 - After) div Minimum,
  Under1 = Code1 - (Code1 - After) rem Minimum,
  Loop1Ctx1 = Loop1Ctx#enet_rc_dec_loop1{value = Value1, under = Under1},
  Context1 = enet_symbol_create(Context, Loop1Ctx1#enet_rc_dec_loop1.value, Update),
  Loop1Ctx2 = Loop1Ctx1#enet_rc_dec_loop1{symbol = Context1#enet_range_coder.currentSymbol},
  Node1 = Node#enet_symbol{right = Loop1Ctx2#enet_rc_dec_loop1.symbol - Loop1Ctx2#enet_rc_dec_loop1.decodeNode},
  Symbols1 = orddict:update(Loop1Ctx2#enet_rc_dec_loop1.decodeNode, fun(_OldSymbol) -> Node1 end,
    Context1#enet_range_coder.symbols),
  Context2 = Context1#enet_range_coder{symbols = Symbols1},
  {Context2, Loop1Ctx2}.


enet_create_left(Context, Loop1Ctx, Update, Minimum, After, Before) ->
  Node = orddict:fetch(Loop1Ctx#enet_rc_dec_loop1.decodeNode, Context#enet_range_coder.symbols),
  Code1 = Loop1Ctx#enet_rc_dec_loop1.code,
  Value1 = Node#enet_symbol.value - 1 - (After - Before - Code1 - 1) div Minimum,
  Under1 = Code1 - (After - Before - Code1 - 1) rem Minimum,
  Loop1Ctx1 = Loop1Ctx#enet_rc_dec_loop1{value = Value1, under = Under1},
  Context1 = enet_symbol_create(Context, Loop1Ctx1#enet_rc_dec_loop1.value, Update),
  Loop1Ctx2 = Loop1Ctx1#enet_rc_dec_loop1{symbol = Context1#enet_range_coder.currentSymbol},
  Node1 = Node#enet_symbol{left = Loop1Ctx2#enet_rc_dec_loop1.symbol - Loop1Ctx2#enet_rc_dec_loop1.decodeNode},
  Symbols1 = orddict:update(Loop1Ctx2#enet_rc_dec_loop1.decodeNode, fun(_OldSymbol) -> Node1 end,
    Context1#enet_range_coder.symbols),
  Context2 = Context1#enet_range_coder{symbols = Symbols1},
  {Context2, Loop1Ctx2}.


enet_symbol_create(Context, Value, Count) ->
  CurrentSymbol = Context#enet_range_coder.nextSymbol,
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
  end, Context#enet_range_coder.symbols),
  Context#enet_range_coder{symbols = Symbols_1, currentSymbol = CurrentSymbol, nextSymbol = CurrentSymbol+1}.


enet_context_create(Context, Escapes, Minimum) ->
  Context1 = enet_symbol_create(Context, 0, 0),
  CurrentSymbol = Context1#enet_range_coder.currentSymbol,
  NewSymbols = orddict:update(CurrentSymbol, fun(OldSymbol) ->
    OldSymbol#enet_symbol{
      escapes = Escapes,
      total = Escapes + 256 * Minimum,
      symbols = 0
      }
  end, Context1#enet_range_coder.symbols),
  Context1#enet_range_coder{symbols = NewSymbols}.


enet_range_coder_seed(Context, Payload) ->
  D1 = Context#enet_range_coder.decodeCode,
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
  {Context#enet_range_coder{decodeCode = D5}, P5}.


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

