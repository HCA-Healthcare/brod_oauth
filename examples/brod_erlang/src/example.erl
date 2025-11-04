-module(example).

-export([main/1, test/1, oauth_params/1]).

get_config() ->
    #{
        bootstrap_endpoints => [{"localhost", 9094}],
        topic => <<"brod.oauth.test">>,
        partition => 0,
        brod_config => [{allow_topic_auto_creation, true}, {auto_start_producers, true}, {connect_timeout, 60000}, {sasl, {callback, brod_oauth, #{token_callback => fun example:oauth_params/1}}}],
        oauth => #{
            url => "http://localhost:8080/realms/waterpark-keycloak/protocol/openid-connect/token",
            grant_type => "client_credentials",
            client_secret => os:getenv("KC_KAFKA_BROKER_SECRET"),
            client_id => "kafka-broker"
        }
    }.

oauth_params(_) ->
    ClientConfig = get_config(),
    OauthConfig = maps:get(oauth, ClientConfig),

    BodyParams = [
        {"grant_type", maps:get(grant_type, OauthConfig)},
        {"client_secret", maps:get(client_secret, OauthConfig)},
        {"client_id", maps:get(client_id, OauthConfig)}
    ],

    BodyParams1 =
        case maps:get(scope, OauthConfig, undefined) of
            undefined ->
                BodyParams;
            Scope ->
                [{"scope", Scope} | BodyParams]
        end,

    Body = uri_string:compose_query(BodyParams1),

    Res = httpc:request(
        post,
        {maps:get(url, OauthConfig), [], "application/x-www-form-urlencoded", Body},
        [],
        [{body_format, binary}]
    ),

    case Res of
        {ok, {{"HTTP/1.1", 200, "OK"}, _headers, Json}} ->
            #{<<"access_token">> := Token} = jsone:decode(Json),
            {ok, #{
                token => Token
            }};
        Err ->
            Err
    end.

sync_offset(ClientName, Topic, Part, Key, Value) ->
    case brod:produce_sync_offset(ClientName, Topic, Part, Key, Value) of
        {ok, _} = Res ->
            Res;
        _Err ->
            timer:sleep(100),
            sync_offset(ClientName, Topic, Part, Key, Value)
    end.

get_producer(ClientName, Topic, Part) ->
    case brod:get_producer(ClientName, Topic, Part) of
        {ok, _} = Res ->
            Res;
        _Err ->
            timer:sleep(100),
            get_producer(ClientName, Topic, Part)
    end.

produce_messages(ClientName, Topic, Partition, Amount, DelayBetween) ->
    I = integer_to_binary(rand:uniform(100_000)),
    [produce_message(ClientName, Topic, Partition, I, DelayBetween) || _X <- lists:seq(1, Amount)].

produce_message(ClientName, Topic, Partition, I, DelayBetween) ->
    F1 = <<I/binary, "Key", I/binary>>,
    F2 = base64:encode(rand:bytes(64)),
    {ok, Offset} = sync_offset(ClientName, Topic, Partition, F1, F2),
    timer:sleep(DelayBetween),
    #{offset => Offset, key => F1, value => F2}.

start_client_and_producer(KafkaBootstrapEndpoints, ClientName, BrodConfig, Topic, Partition, ProducerConfig) ->
    try
        ok = brod:start_client(KafkaBootstrapEndpoints, ClientName, BrodConfig),
        ok = brod:start_producer(ClientName, Topic, ProducerConfig),
        {ok, _Pid} = get_producer(ClientName, Topic, Partition)
    catch
        _ErrorClass:_Reason:_Stack ->
            brod:stop_client(ClientName),
            timer:sleep(10),
            start_client_and_producer(KafkaBootstrapEndpoints, ClientName, BrodConfig, Topic, Partition, ProducerConfig)
    end.

main(_) ->
    test(keycloak).

test(TestType) ->
    info_log("Starting test for " ++ atom_to_list(TestType)),
    ClientName = client1,
    ClientConfig = get_config(),
    logger:set_primary_config(level, warn),

    try
        os:putenv("NIF_BIN_DIR", "_build/default/lib/crc32cer/priv"),
        ensure_started([inets, ssl, brod]),
        KafkaBootstrapEndpoints = maps:get(bootstrap_endpoints, ClientConfig),
        Topic = maps:get(topic, ClientConfig),
        Partition = maps:get(partition, ClientConfig),
        BrodConfig = maps:get(brod_config, ClientConfig),
        ProducerConfig = [{max_retries, 100}],
        info_log("Starting brod client and producer..."),
        start_client_and_producer(KafkaBootstrapEndpoints, ClientName, BrodConfig, Topic, Partition, ProducerConfig),
        info_log("Brod client and producer are ready."),
        DelayBetween = 10,
        Amount = 1000,

        info_log("Producing "
                 ++ integer_to_list(Amount) ++
                 " messages with "
                 ++ integer_to_list(DelayBetween)
                 ++ "ms delay between each message."
        ),
        [#{offset := FirstOffset} | _] =
            Messages = produce_messages(ClientName, Topic, Partition, Amount, DelayBetween),

        SubscriberCallbackFun = fun(_Partition, Msg, ShellPid = CallbackState) ->
            ShellPid ! Msg,
            {ok, ack, CallbackState}
        end,

        Receive = fun() ->
            receive
                Msg ->
                    Msg
            after 5000 -> timeout
            end
        end,

        info_log("All message successfully produced."),
        info_log("Starting consumer..."),

        {ok, Pid} = brod_topic_subscriber:start_link(
            ClientName,
            Topic,
            _Partitions = [Partition],
            _ConsumerConfig = [{begin_offset, FirstOffset}],
            _CommittdOffsets = [],
            message,
            SubscriberCallbackFun,
            _CallbackState = self()
        ),

        info_log("Consumer started."),

        MatchMessage = fun(#{offset := Offset, key := Key, value := Value}) ->
            {kafka_message, Offset, Key, Value, _, _, _} = Receive()
        end,

        [MatchMessage(X) || X <- Messages],

        info_log("All messages successfully consumed."),
        success_log("Produced and consumed " ++ integer_to_list(Amount) ++ " messages."),
        brod_topic_subscriber:stop(Pid),
        brod:stop_client(ClientName),
        erlang:halt()
    catch
        ErrorClass:Reason:Stack ->
            fail_log(ErrorClass, Reason, Stack)
    end.

info_log(Msg) ->
    io:format("\033[0;96mINFO    \033[0m:  ~s~n", [Msg]).

success_log(Msg) ->
    io:format("\033[0;32mSUCCESS\033[0m : ~s~n", [Msg]).

fail_log(ErrorClass, Reason, Stack) ->
    io:format(
        "\033[0;31mFAIL\033[0m          : Failed to send and receive messages with brod => ~p:~p:~p~n~n~n~n~n",
        [ErrorClass, Reason, Stack]
    ).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 26).
ensure_started(Apps) ->
    {ok, _} = application:ensure_all_started(Apps).
-else.
ensure_started(Apps) ->
  [application:ensure_all_started(App) || App <- Apps].
-endif.
-endif.
