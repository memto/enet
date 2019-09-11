%%
%% Send Reliable Command
%%

-record(reliable,
        {
          data = <<>>
        }).


%%
%% Send Unreliable Command
%%

-record(unreliable,
        {
          sequence_number = 0,
          data            = <<>>
        }).


%%
%% Send Unsequenced Command
%%

-record(unsequenced,
        {
          group = 0,
          data  = <<>>
        }).

-define(ENET_TIME_OVERFLOW, 86400000).
-define(ENET_TIME_LESS(A, B), ((A) - (B) >= ?ENET_TIME_OVERFLOW)).
-define(ENET_TIME_GREATER(A, B), ((B) - (A) >= ?ENET_TIME_OVERFLOW)).
-define(ENET_TIME_LESS_EQUAL(A, B), (! ?ENET_TIME_GREATER (A, B))).
-define(ENET_TIME_GREATER_EQUAL(A, B), (! ?ENET_TIME_LESS (A, B))).
-define(ENET_TIME_DIFFERENCE(A, B), (case ((A) - (B) >= ?ENET_TIME_OVERFLOW) of true -> ((B) - (A)); false -> ((A) - (B)) end)).