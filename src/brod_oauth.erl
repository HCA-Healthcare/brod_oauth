-module(brod_oauth).

-moduledoc false.

-on_load(init/0).

-export([auth/7, new/7]).

-type state() :: #{
    host := binary(),
    sock := gen_tcp:socket() | ssl:sslsocket(),
    transport_mod := gen_tcp | ssl,
    client_id := binary(),
    timeout := pos_integer(),
    method := any(),
    handshake_vsn := non_neg_integer() | legacy
}.

-export_type([state/0]).

-spec auth(
    Host :: string(),
    Sock :: gen_tcp:socket() | ssl:sslsocket(),
    HandshakeVsn :: non_neg_integer(),
    Mod :: gen_tcp | ssl,
    ClientId :: binary(),
    Timeout :: pos_integer(),
    OauthOpts :: map()
) -> ok | {error, Reason :: term()}.
auth(
    Host,
    Sock,
    HandshakeVsn,
    Mod,
    ClientId,
    Timeout,
    Opts
) ->
    State = new(Host, Sock, HandshakeVsn, Mod, ClientId, Timeout, Opts),
    dispatch(State).

dispatch(#{handshake_vsn := 1} = State) ->
    brod_oauth_v1:auth(State);
dispatch(#{handshake_vsn := 0} = _State) ->
    {error, <<"v0 handshake not implemented">>};
dispatch(#{handshake_vsn := legacy} = _State) ->
    {error, <<"v0 handshake not implemented">>};
dispatch(_State) ->
    {error, undefined_handshake_vsn}.

-spec new(
    Host :: string(),
    Sock :: gen_tcp:socket() | ssl:sslsocket(),
    HandshakeVsn :: non_neg_integer(),
    Mod :: gen_tcp | ssl,
    ClientId :: binary(),
    Timeout :: pos_integer(),
    OauthOpts :: map()
) -> state().
new(Host, Sock, HandshakeVsn, Mod, ClientId, Timeout, #{token_callback := OAuthCB} = Cfg) ->
    #{
        debug => maps:get(debug, Cfg, false),
        host => ensure_binary(Host),
        sock => Sock,
        transport_mod => Mod,
        client_id => ClientId,
        timeout => Timeout,
        handshake_vsn => HandshakeVsn,
        token_callback => OAuthCB
    }.

-spec ensure_binary(atom() | iodata()) -> binary().
ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
ensure_binary(Str) when is_list(Str) ->
    iolist_to_binary(Str);
ensure_binary(Bin) when is_binary(Bin) ->
    Bin.

init() ->
    application:load(brod_oauth),
    ok.
