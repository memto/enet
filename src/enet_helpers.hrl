-define(DBG_Y(X), io:fwrite(X)).
-define(DBG_Y(X, Y), io:fwrite(X, Y)).
-define(DBG_N(X), ok).
-define(DBG_N(X, Y), ok).

-define(DBG_HOST(X), ?DBG_N(X)).
-define(DBG_HOST(X, Y), ?DBG_N(X, Y)).

-define(DBG_CHECKSUM(X), ?DBG_N(X)).
-define(DBG_CHECKSUM(X, Y), ?DBG_N(X, Y)).

-define(DBG_PACKET(X), ?DBG_N(X)).
-define(DBG_PACKET(X, Y), ?DBG_N(X, Y)).

-define(DBG_MISC(X), ?DBG_N(X)).
-define(DBG_MISC(X, Y), ?DBG_N(X, Y)).

-define(DBG_PEER(X), ?DBG_N(X)).
-define(DBG_PEER(X, Y), ?DBG_N(X, Y)).

-define(DBG_PEER_HS(X), ?DBG_N(X)).
-define(DBG_PEER_HS(X, Y), ?DBG_N(X, Y)).

-define(DBG_RESENT(X), ?DBG_N(X)).
-define(DBG_RESENT(X, Y), ?DBG_N(X, Y)).