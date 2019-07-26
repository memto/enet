-define(ENET_RANGE_CODER_TOP          , (1 bsl 24)).
-define(ENET_RANGE_CODER_BOTTOM       , (1 bsl 16)).

-define(ENET_CONTEXT_SYMBOL_DELTA     , 3).
-define(ENET_CONTEXT_SYMBOL_MINIMUM   , 1).
-define(ENET_CONTEXT_ESCAPE_MINIMUM   , 1).

-define(ENET_SUBCONTEXT_ORDER         , 2).
-define(ENET_SUBCONTEXT_SYMBOL_DELTA  , 2).
-define(ENET_SUBCONTEXT_ESCAPE_DELTA  , 5).

-define(MAX_UINT32                    , 4294967295).


-record(enet_symbol,
        {
          value = 0,
          count = 0,
          under = 0,
          left = 0,
          right = 0,

          symbols = 0,
          escapes = 0,
          total = 0,
          parent = 0
        }).

-record(enet_rc_dec_vars,
        {
          symbols,

          decodeLow,
          decodeCode,
          decodeRange,

          predicted,
          order,
          currentSymbol,
          nextSymbol
        }).

-record(enet_rc_dec_loop1_vars,
        {
          subcontext,
          symbol,
          patch,

          value,
          code,
          under,
          count,
          bottom,
          parent,
          total,

          decodeNode,
          encodeNode
        }).

-record(enet_compressor, {
        context,
        compress,
        decompress,
        destroy
       }).