-define(DBG(X), io:fwrite(X)).
-define(DBG(X, Y), io:fwrite(X, Y)).

% -define(DBG_HOST(X), ?DBG(X)).
% -define(DBG_HOST(X, Y), ?DBG(X, Y)).
-define(DBG_HOST(X), ok).
-define(DBG_HOST(X, Y), ok).

-define(DBG_CHECKSUM(X), ok).
-define(DBG_CHECKSUM(X, Y), ok).

-define(DBG_PACKET(X), ok).
-define(DBG_PACKET(X, Y), ok).

% -define(DBG_MISC(X), ?DBG(X)).
% -define(DBG_MISC(X, Y), ?DBG(X, Y)).
-define(DBG_MISC(X), ok).
-define(DBG_MISC(X, Y), ok).

-define(DBG_PEER(X), ?DBG(X)).
-define(DBG_PEER(X, Y), ?DBG(X, Y)).
% -define(DBG_PEER(X), ok).
% -define(DBG_PEER(X, Y), ok).

% -define(DBG_PEER_HS(X), ?DBG(X)).
% -define(DBG_PEER_HS(X, Y), ?DBG(X, Y)).
-define(DBG_PEER_HS(X), ok).
-define(DBG_PEER_HS(X, Y), ok).