-module(brod_oauth_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%%%%%%%%%%%%%%%%
%%%  CT hooks  %%%
%%%%%%%%%%%%%%%%%%

all() ->
    [
        simple,
        handshake_failure,
        handshake_failure_unknown_error,
        exchange_failure,
        error_on_callback,
        bad_return_from_callback,
        exit_on_callback,
        throw_from_callback
    ].

-define(MOCK_MODULES, [inet, ssl, kpro_req_lib, kpro_lib, kpro]).
-define(TOKEN,
        <<"eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIiwia2lkIiA6ICJCaDI0MmdQejZCRlQ3aGxOV3Jma3VXa2duQ1U3eUdJZFZtV0QzSEdTTHdRIn0.eyJleHAiOjE3MjM1NDYwMjUsImlhdCI6MTcyMzU0NTcyNSwianRpIjoiNmRlYTQxNGEtYzU5Yy00ODRhLTg4MTctNDk2ZmViNWQ0MGY3IiwiaXNzIjoiaHR0cDovL2xvY2FsaG9zdDo4MDgwL3JlYWxtcy93YXRlcnBhcmsta2V5Y2xvYWsiLCJhdWQiOiJhY2NvdW50Iiwic3ViIjoiZmMzMGNhNjYtYjM5NC00ZTdkLWJiNzMtOTg2ZTdiOWYyMmQyIiwidHlwIjoiQmVhcmVyIiwiYXpwIjoia2Fma2EtYnJva2VyIiwiYWNyIjoiMSIsImFsbG93ZWQtb3JpZ2lucyI6WyIvKiJdLCJyZWFsbV9hY2Nlc3MiOnsicm9sZXMiOlsib2ZmbGluZV9hY2Nlc3MiLCJkZWZhdWx0LXJvbGVzLXdhdGVycGFyay1rZXljbG9hayIsInVtYV9hdXRob3JpemF0aW9uIl19LCJyZXNvdXJjZV9hY2Nlc3MiOnsiYWNjb3VudCI6eyJyb2xlcyI6WyJtYW5hZ2UtYWNjb3VudCIsIm1hbmFnZS1hY2NvdW50LWxpbmtzIiwidmlldy1wcm9maWxlIl19fSwic2NvcGUiOiJwcm9maWxlIGVtYWlsIiwiZW1haWxfdmVyaWZpZWQiOmZhbHNlLCJjbGllbnRIb3N0IjoiMTkyLjE2OC42NS4xIiwicHJlZmVycmVkX3VzZXJuYW1lIjoic2VydmljZS1hY2NvdW50LWthZmthLWJyb2tlciIsImNsaWVudEFkZHJlc3MiOiIxOTIuMTY4LjY1LjEiLCJjbGllbnRfaWQiOiJrYWZrYS1icm9rZXIifQ.og6l8iQkUrnC5zZDnA4ikfLNVQuvU1u1IxDF5n6Pjq6-dMkcBCeEG7ZJK2boGSesNwR3dILVQm1a30ydeP6q79HFveRojz54vMtyOR9rtD3WJPkMlSLzmWot9WT78Ozjh7BJv-omYIAdAhF2CQrp1XPCtipnLZm0L_M6sneaCdrOk4M4kct5kNFQorFI6q4hmHvQY06qKwFy6LGGZG2y9CpjmFdFraR8J2RoxOPlgy1_DVbeGxcDXZ5nNc0sdVuppDONnG1IFGlaPiNpKQmGc43DVBXrKds3IutQeCsq3nIe8ewYE-kuKQW7xtAVpcMPvqnO84ezV1IsuOKBFfs6Og">>).

-define(CH_BINDING, <<"n">>).
-define(SEP, <<0001/utf8>>).
-define(SCHEME, <<"auth=Bearer ">>).
-define(AUTHZ, <<",,">>).
-define(EXTS, <<"">>).
-define(EXCHANGE,<<?CH_BINDING/binary, ?AUTHZ/binary, ?SEP/binary, ?SCHEME/binary, ?TOKEN/binary, ?EXTS/binary,
                   ?SEP/binary, ?SEP/binary>>).

init_per_suite(Config) ->
    meck:new(
        ?MOCK_MODULES,
        [passthrough, no_link, unstick]
    ),
    Config.

end_per_suite(Config) ->
    meck:unload(?MOCK_MODULES),
    Config.

init_per_testcase(_Tc, Config) ->
    Config.

end_per_testcase(Tc, Config) ->
    validate_mocks(?MOCK_MODULES) orelse erlang:error(Tc ++ " failed meck validation"),
    reset_mocks(?MOCK_MODULES),
    Config.

simple(_Config) ->

    meck:expect(
        kpro_req_lib,
        make,
        fun
            (sasl_handshake, 1, [{mechanism,<<"OAUTHBEARER">>}]) ->
                req1;
            (sasl_authenticate, 1, [{auth_bytes, Exchange}]) ->
                Exchange = ?EXCHANGE,
                req2
           end
    ),

    meck:expect(kpro_lib, send_and_recv, fun(_, _, _, _, _) -> rsp end),
    meck:expect(
        kpro,
        find,
        fun
            (error_code, rsp) ->
                no_error;
            (auth_bytes, rsp) ->
                <<"auth_bytes">>
        end
    ),
    meck:expect(inet, setopts, fun(_, _) -> ok end),
    ?assertMatch(
       {ok, rsp},
        brod_oauth:auth(
            "host",
            make_ref(),
            1,
            gen_tcp,
            <<"client_id">>,
            42,
            #{token_callback => fun (_) -> {ok, #{token => ?TOKEN}} end}
        )
    ).

handshake_failure(_Config) ->
    meck:expect(
        kpro_req_lib,
        make,
        fun
            (sasl_handshake, 1, [{mechanism,<<"OAUTHBEARER">>}]) ->
                req1
           end
    ),
    meck:expect(
        kpro_lib,
        send_and_recv,
        fun
            (req1, _, _, _, _) ->
                rsp1
        end
    ),

    meck:expect(
        kpro,
        find,
        fun
            (error_code, rsp1) ->
                unsupported_sasl_mechanism;
            (enabled_mechanisms, rsp1) ->
                ["foo", "bar", "baz"];
            (auth_bytes, _) ->
                <<"auth_bytes">>
        end
    ),

    HandshakeErr = {error,<<"sasl mechanism OAUTHBEARER is not enabled in kafka, enabled mechanism(s): foo,bar,baz">>},

    meck:expect(inet, setopts, fun(_, _) -> ok end),
    ?assertMatch(
       HandshakeErr,
        brod_oauth:auth(
            "host",
            make_ref(),
            1,
            gen_tcp,
            <<"client_id">>,
            42,
            #{token_callback => fun (_) -> {ok, #{token => <<"ABCDEFG">>}} end}
        )
    ).

handshake_failure_unknown_error(_Config) ->
    meck:expect(
        kpro_req_lib,
        make,
        fun
            (sasl_handshake, 1, [{mechanism,<<"OAUTHBEARER">>}]) ->
                req1
           end
    ),
    meck:expect(
        kpro_lib,
        send_and_recv,
        fun
            (req1, _, _, _, _) ->
                rsp1
        end
    ),

    meck:expect(
        kpro,
        find,
        fun
            (error_code, rsp1) ->
                unknown_error;
            (auth_bytes, _) ->
                <<"auth_bytes">>
        end
    ),

    meck:expect(inet, setopts, fun(_, _) -> ok end),
    ?assertMatch(
       {error, unknown_error},
        brod_oauth:auth(
            "host",
            make_ref(),
            1,
            gen_tcp,
            <<"client_id">>,
            42,
            #{token_callback => fun (_) -> {ok, #{token => <<"ABCDEFG">>}} end}
        )
    ).

exchange_failure(_Config) ->
    meck:expect(
        kpro_req_lib,
        make,
        fun
            (sasl_handshake, 1, [{mechanism,<<"OAUTHBEARER">>}]) ->
                req1;
            (sasl_authenticate, 1, [{auth_bytes, _Exchange}]) ->
                req2
           end
    ),
    meck:expect(
        kpro_lib,
        send_and_recv,
        fun
            (req1, _, _, _, _) ->
                rsp1;
             (req2, _, _, _, _) ->
                rsp2
        end
    ),

    AuthErr =  {{sasl_auth_error,<<"Authentication failed during authentication due to invalid credentials with SASL mechanism OAUTHBEARER">>}},
    meck:expect(
        kpro,
        find,
        fun
            (error_code, rsp1) ->
                no_error;
            (error_code, rsp2) ->
                error;
            (error_message, rsp2) ->
                AuthErr;
            (auth_bytes, _) ->
                <<"auth_bytes">>
        end
    ),

    meck:expect(inet, setopts, fun(_, _) -> ok end),
    ?assertMatch(
       {error, AuthErr},
        brod_oauth:auth(
            "host",
            make_ref(),
            1,
            gen_tcp,
            <<"client_id">>,
            42,
            #{token_callback => fun (_) -> {ok, #{token => <<"ABCDEFG">>}} end}
        )
    ).


error_on_callback(_Config) ->
    meck:expect(kpro_req_lib, make, fun(_, _, _) -> req end),
    meck:expect(kpro_lib, send_and_recv, fun(_, _, _, _, _) -> rsp end),
    meck:expect(
        kpro,
        find,
        fun
            (error_code, rsp) ->
                no_error;
            (auth_bytes, rsp) ->
                <<"auth_bytes">>
        end
    ),

    meck:expect(inet, setopts, fun(_, _) -> ok end),
    ?assertMatch(
       {error, whoops},
        brod_oauth:auth(
            "host",
            make_ref(),
            1,
            gen_tcp,
            <<"client_id">>,
            42,
            #{token_callback => fun (_) -> erlang:error(whoops) end}
        )
    ).

bad_return_from_callback(_Config) ->
    meck:expect(kpro_req_lib, make, fun(_, _, _) -> req end),
    meck:expect(kpro_lib, send_and_recv, fun(_, _, _, _, _) -> rsp end),
    meck:expect(
        kpro,
        find,
        fun
            (error_code, rsp) ->
                no_error;
            (auth_bytes, rsp) ->
                <<"auth_bytes">>
        end
    ),

    meck:expect(inet, setopts, fun(_, _) -> ok end),
    ?assertMatch(
       {error, _},
        brod_oauth:auth(
            "host",
            make_ref(),
            1,
            gen_tcp,
            <<"client_id">>,
            42,
            #{token_callback => fun (_) -> ?TOKEN end}
        )
    ).

exit_on_callback(_Config) ->
    meck:expect(kpro_req_lib, make, fun(_, _, _) -> req end),
    meck:expect(kpro_lib, send_and_recv, fun(_, _, _, _, _) -> rsp end),
    meck:expect(
        kpro,
        find,
        fun
            (error_code, rsp) ->
                no_error;
            (auth_bytes, rsp) ->
                <<"auth_bytes">>
        end
    ),

    meck:expect(inet, setopts, fun(_, _) -> ok end),
    ?assertMatch(
       {error, whoops},
        brod_oauth:auth(
            "host",
            make_ref(),
            1,
            gen_tcp,
            <<"client_id">>,
            42,
            #{token_callback => fun (_) -> erlang:exit(whoops) end}
        )
    ).

throw_from_callback(_Config) ->
    meck:expect(kpro_req_lib, make, fun(_, _, _) -> req end),
    meck:expect(kpro_lib, send_and_recv, fun(_, _, _, _, _) -> rsp end),
    meck:expect(
        kpro,
        find,
        fun
            (error_code, rsp) ->
                no_error;
            (auth_bytes, rsp) ->
                <<"auth_bytes">>
        end
    ),

    meck:expect(inet, setopts, fun(_, _) -> ok end),
    ?assertMatch(
       {error, thrown, {ok, _}},
        brod_oauth:auth(
            "host",
            make_ref(),
            1,
            gen_tcp,
            <<"client_id">>,
            42,
            #{token_callback => fun (_) -> throw({ok, #{token => <<"ABCDEFG">>}}) end}
        )
    ).


%%%%%%%%%%%%%%%%%%
%%%  Helpers   %%%
%%%%%%%%%%%%%%%%%%

validate_mocks(Modules) ->
    lists:all(fun(Mod) -> meck:validate(Mod) end, Modules).

reset_mocks(Modules) ->
    meck:reset(Modules),
    [
        meck:delete(Module, Fun, Arity, false)
     || {Module, Fun, Arity} <- meck:expects(Modules, true)
    ].
