-module(brod_oauth_v1).

-moduledoc false.

-export([auth/1]).

-define(HANDSHAKE_V1, 1).
-define(MECH, <<"OAUTHBEARER">>).

-define(DBG(DEBUG_ON, MSG),
    case DEBUG_ON of
        true ->
            io:format("\033[0;93mDEBUG  : \033[0m\033[0;97m ~p:~p - ~s \033[0m ~n", [
                ?MODULE, ?LINE, MSG
            ]);
        false ->
            ok
    end
).

-define(DBG(DEBUG_ON, MSG, DATA),
    case DEBUG_ON of
        true ->
            io:format("\033[0;93mDEBUG  : \033[0m\033[0;97m ~p:~p - ~s => ~p \033[0m ~n", [
                ?MODULE, ?LINE, MSG, DATA
            ]);
        false ->
            ok
    end
).

-spec auth(brod_oauth:state()) -> {ok, Result :: term()} | {error, Reason :: term()}.
auth(State) ->
    case auth_begin(State) of
        ok ->
            auth_continue(State);
        Error ->
            Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec auth_begin(State :: brod_oauth:state()) -> ok | {error, term()}.
auth_begin(#{debug := Debug} = State) ->
    ?DBG(Debug, "Attempting sasl handshake..."),
    case handshake(State) of
        ok ->
            ?DBG(Debug, "Handshake successful"),
            ok;
        Error ->
            Error
    end.

-spec auth_continue(State :: brod_oauth:state()) ->
    ok | {error, term()}.
auth_continue(#{token_callback := TokenCB, debug := Debug} = State) ->
    ?DBG(Debug, "Preparing to fire get_auth_data callback..."),

    #{
        host := Host,
        client_id := ClientId,
        timeout := Timeout,
        debug := Debug
    } = State,

    CallbackData = #{host => Host, client_id => ClientId, timeout => Timeout},

    ?DBG(Debug, "callback data to be passed", CallbackData),

    case do_get_token_props(State, TokenCB, CallbackData, Debug) of
        {ok, TokenProps} ->
            ?DBG(Debug, "Attempting to send token and auth properties..."),
            case send_sasl_token(State, TokenProps) of
                {ok, _} = Res ->
                    ?DBG(Debug, "Successfully authenticated."),
                    _ = set_sock_opts(State, [{active, once}]),
                    Res;
                Error ->
                    Error
            end;
        Err ->
            Err
    end.

do_get_token_props(State, Callback, CallbackData, Debug) ->
    try Callback(CallbackData) of
        {ok, Props} when is_map(Props) ->
            ?DBG(Debug, "Successfully fired auth data callback"),
            validate_auth_props(State, Props, Debug);
        {ok, Other} ->
            Msg = iolist_to_binary(io_lib:format(
                <<"Expected a map from brod_oauth token callback, got : ~0p">>,
                [Other]
            )),
            {error, Msg};
        Other ->
            {error, Other}
    catch
        error:Err ->
            {error, Err};
        exit:Err ->
            {error, Err};
        Thrown ->
            {error, thrown, Thrown}
    end.

% TODO: Validate all possible props
validate_auth_props(State, #{token := Token} = Props, Debug) when is_binary(Token) ->
    ?DBG(Debug, "Auth data is valid."),
    {ok, set_defaults(State, Props)};
validate_auth_props(_, _, Debug) ->
    ?DBG(Debug, "Auth data is invalid."),
    {error, "Missing binary token associated with key token"}.

set_defaults(State, #{token := Token}) ->
    #{
        authz_id => maps:get(authz_id, State),
        extensions => maps:get(extensions, State),
        token => Token
    }.

-dialyzer({nowarn_function, set_sock_opts/2}).
-spec set_sock_opts(State :: brod_oauth:state(), [gen_tcp:option()]) -> ok | {error, inet:posix()}.
set_sock_opts(#{sock := Sock, transport_mod := gen_tcp}, Opts) ->
    inet:setopts(Sock, Opts);
set_sock_opts(#{sock := Sock, transport_mod := ssl}, Opts) ->
    ssl:setopts(Sock, Opts).

-spec handshake(State :: brod_oauth:state()) -> ok | {error, term()}.
handshake(State) ->
    #{handshake_vsn := HandshakeVsn, timeout := Timeout} = State,
    #{sock := Sock, transport_mod := Mod, client_id := ClientId} = State,
    Req = kpro_req_lib:make(sasl_handshake, HandshakeVsn, [{mechanism, ?MECH}]),
    Rsp = kpro_lib:send_and_recv(Req, Sock, Mod, ClientId, Timeout),
    case kpro:find(error_code, Rsp) of
        no_error ->
            ok;
        unsupported_sasl_mechanism ->
            EnabledMechanisms = kpro:find(enabled_mechanisms, Rsp),
            Msg = io_lib:format(
                "sasl mechanism ~s is not enabled in "
                "kafka, enabled mechanism(s): ~s",
                [?MECH, string:join(EnabledMechanisms, ",")]
            ),
            {error, iolist_to_binary(Msg)};
        Other ->
            {error, Other}
    end.
-spec send_sasl_token(State :: brod_oauth:state(), TokenProps :: map()) ->
    {ok, term()} | {error, term()}.
send_sasl_token(State, TokenProps) ->
    #{handshake_vsn := HandshakeVsn, timeout := Timeout} = State,
    #{sock := Sock, transport_mod := Mod, client_id := ClientId} = State,

    Challenge = make_gs2_context_token(TokenProps),
    Req = kpro_req_lib:make(sasl_authenticate, HandshakeVsn, [{auth_bytes, Challenge}]),
    Rsp = kpro_lib:send_and_recv(Req, Sock, Mod, ClientId, Timeout),
    case kpro:find(error_code, Rsp) of
        no_error ->
            {ok, Rsp};
        _ ->
            {error, kpro:find(error_message, Rsp)}
    end.

%% Kafka support oauth via gs2 context token exchange.
%% See https://www.rfc-editor.org/rfc/rfc5801.html for details on format

-define(COMMA, <<",">>).
-define(SEP, <<0001/utf8>>).
-define(BREAK, <<>>).
-define(SCHEME, <<"auth=Bearer ">>).

%% While channel binding is required per rfc5801, it is currently not supported by Kafka.
-define(CH_BINDING, <<"n">>).

make_gs2_context_token(#{token := Token, authz_id := AuthzId, extensions := Exts}) ->
    Extensions = make_extensions(maps:to_list(Exts)),

    <<?CH_BINDING/binary, ?COMMA/binary, AuthzId/binary, ?COMMA/binary, ?SEP/binary, ?SCHEME/binary, Token/binary,
        ?SEP/binary, Extensions/binary, ?SEP/binary>>.

make_extensions(Exts) ->
    make_extensions(Exts, <<"">>).

make_extensions([], Acc) ->
    Acc;
make_extensions([{K, V} | Rest], Acc) ->
    NewAcc = <<Acc/binary, K/binary, "=", V/binary, ?BREAK/binary, ?SEP/binary>>,
    make_extensions(Rest, NewAcc).
