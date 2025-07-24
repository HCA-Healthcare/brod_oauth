defmodule Example do
  require Logger

  def get_config() do
    %{
      bootstrap_endpoints: [{"localhost", 9094}],
      topic: <<"brod.oauth.test">>,
      partition: 0,
      brod_config: [
        connect_timeout: 60_000,
        sasl: {:callback, :brod_oauth, %{token_callback: &Example.oauth_params/1}}
      ],
      oauth: %{
        url: "http://localhost:8080/realms/waterpark-keycloak/protocol/openid-connect/token",
        grant_type: "client_credentials",
        client_secret: System.get_env("KC_KAFKA_BROKER_SECRET"),
        client_id: "kafka-broker"
      }
    }
  end

  def oauth_params(_) do
    client_config = get_config()
    oauth_config = client_config.oauth
    extensions = Map.get(client_config, :extensions, %{})

    body_params = %{
      "grant_type" => oauth_config.grant_type,
      "client_secret" => oauth_config.client_secret,
      "client_id" => oauth_config.client_id
    }

    body_params1 =
      case oauth_config[:scope] do
        nil ->
          body_params

        scope ->
          Map.put(body_params, "scope", scope)
      end

    body = URI.encode_query(body_params1)

    res =
      :httpc.request(
        :post,
        {to_charlist(oauth_config.url), [], ~c"application/x-www-form-urlencoded",
         to_charlist(body)},
        [],
        [{:body_format, :binary}]
      )

    case res do
      {:ok, {{~c"HTTP/1.1", 200, ~c"OK"}, _headers, json}} ->
        %{"access_token" => token} = Jason.decode!(json)

        {:ok,
         %{
           token: token,
           extensions: extensions
         }}

      err ->
        err
    end
  end

  def sync_offset(client_name, topic, part, key, value) do
    case :brod.produce_sync_offset(client_name, topic, part, key, value) do
      {:ok, _} = res ->
        res

      _err ->
        :timer.sleep(100)
        sync_offset(client_name, topic, part, key, value)
    end
  end

  def get_producer(client_name, topic, part) do
    case :brod.get_producer(client_name, topic, part) do
      {:ok, _} = res ->
        res

      _Err ->
        :timer.sleep(100)
        get_producer(client_name, topic, part)
    end
  end

  def produce_messages(client_name, topic, partition, amount, delay_between) do
    i = :erlang.integer_to_binary(:rand.uniform(100_000))
    for _n <- 1..amount, do: produce_message(client_name, topic, partition, i, delay_between)
  end

  def produce_message(client_name, topic, partition, i, delay_between) do
    k = <<i::binary, "Key", i::binary>>
    v = Base.encode64(:rand.bytes(64))
    {:ok, offset} = sync_offset(client_name, topic, partition, k, v)
    :timer.sleep(delay_between)
    %{offset: offset, key: k, value: v}
  end

  def start_client_and_producer(
        kafka_bootstrap_endpoints,
        client_name,
        brod_config,
        topic,
        partition,
        producer_config
      ) do
    try do
      :ok = :brod.start_client(kafka_bootstrap_endpoints, client_name, brod_config)
      :ok = :brod.start_producer(client_name, topic, producer_config)
      {:ok, _pid} = get_producer(client_name, topic, partition)
    rescue
      _ ->
        :brod.stop_client(client_name)
        :timer.sleep(10)

        start_client_and_producer(
          kafka_bootstrap_endpoints,
          client_name,
          brod_config,
          topic,
          partition,
          producer_config
        )
    end
  end

   def test(test_type) do
    System.put_env("NIF_BIN_DIR", "_build/dev/lib/crc32cer/priv")
    Logger.configure(level: :info)
    info_log("Starting test for #{Atom.to_string(test_type)}")
    client_name = :client1
    client_config = get_config()

    try do
      Application.ensure_all_started([:inets, :ssl, :brod])
      kafka_bootstrap_endpoints = client_config.bootstrap_endpoints
      topic = client_config.topic
      partition = client_config.partition
      brod_config = client_config.brod_config
      producer_config = [max_retries: 1000]

      info_log("Starting brod client and producer...")

      start_client_and_producer(
        kafka_bootstrap_endpoints,
        client_name,
        brod_config,
        topic,
        partition,
        producer_config
      )

      info_log("Brod client and producer are ready.")
      delay_between = 10
      amount = 1000

      info_log("Producing #{amount} messages with #{delay_between}ms delay between each message.")

      [%{offset: first_offset} | _] =
        messages = produce_messages(client_name, topic, partition, amount, delay_between)

      subscriber_callback_fun = fn _partition, msg, shell_pid = callback_state ->
        send(shell_pid, msg)
        {:ok, :ack, callback_state}
      end

      info_log("All message successfully produced.")
      info_log("Starting consumer...")

      {:ok, pid} =
        :brod_topic_subscriber.start_link(
          client_name,
          topic,
          _partitions = [partition],
          _consumer_config = [{:begin_offset, first_offset}],
          _committed_offsets = [],
          :message,
          subscriber_callback_fun,
          _callback_state = self()
        )

      info_log("Consumer started.")

      receive_fun = fn ->
        receive do
          msg ->
            msg
        after
          5000 -> :timeout
        end
      end

      match_message = fn %{offset: offset, key: key, value: value} ->
        {:kafka_message, ^offset, ^key, ^value, _, _, _} = receive_fun.()
      end

      for x <- messages, do: match_message.(x)

      info_log("All messages successfully consumed.")
      success_log("Produced and consumed #{amount} messages.")
      :brod_topic_subscriber.stop(pid)
      :brod.stop_client(client_name)
      :erlang.halt()
    rescue
      e ->
        fail_log(e, __STACKTRACE__)
    catch
      type, reason ->
        fail_log(type, reason, __STACKTRACE__)
    end
  end

  def info_log(msg) do
    Logger.info(msg)
  end

  def success_log(msg) do
    Logger.info(IO.ANSI.green() <> msg <> IO.ANSI.reset())
  end

  def fail_log(exception, stack) do
    Logger.error(Exception.format(:error, exception, stack))
  end

  def fail_log(kind, reason, stack) do
    Logger.error(Exception.format(kind, reason, stack))
  end
end
