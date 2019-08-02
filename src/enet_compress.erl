-module(enet_compress).

-include("enet_compress.hrl").

-export([
        trigger_load/0,
        enet_get_compressor/1,
        enet_range_coder_compress/2,
        enet_range_coder_decompress/2, decompress/1,
        enet_range_coder_destroy/1
    ]).

% for debugging
-export([
    % getopts/1,
    progname/0
    ]).

-on_load(on_load/0).

on_load() ->
    % io:fwrite("enet_compress: ~s ~n", [progname()]),
    erlang:load_nif(progname(), []).

trigger_load() ->
    ok.

enet_get_compressor(range_coder) ->
  #enet_compressor {
    context = nil,
    compress = {enet_compress, enet_range_coder_compress, []},
    decompress = {enet_compress, enet_range_coder_decompress, []},
    destroy = {enet_compress, enet_range_coder_destroy, []}
  }.

enet_range_coder_compress(_Ctx, Payload) ->
    {ok, <<>>}.

enet_range_coder_decompress(_Ctx, Payload) ->
    % io:fwrite("~w enet_range_coder_decompress ~n", [?MODULE]),
    decompress(Payload).

decompress(Payload) -> decompress(Payload).

enet_range_coder_destroy(_Ctx) ->
    {ok, <<>>}.

progname_ebin() ->
    filename:join([
        filename:dirname( code:which( ?MODULE ) ),
        "..", "priv", ?MODULE
    ]).

progname_priv() ->
    case application:get_env( ?MODULE, port_executable ) of
        {ok, Executable} -> Executable;
        undefined -> filename:join([
                            code:priv_dir( ?MODULE ),
                            ?MODULE
                        ])
    end.

progname() ->
    % Is there a proper way of getting App-Name in this context?
    case code:priv_dir( ?MODULE ) of
        {error, bad_name} -> progname_ebin();
        _ -> progname_priv()
    end.