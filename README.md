# brod_oauth

[![Build Status](https://github.com/HCA-Healthcare/brod_oauth/actions/workflows/ci.yml/badge.svg)](https://github.com/HCA-Healthcare/brod_oauth/actions/workflows/ci.yml)
[![Hex pm](https://img.shields.io/hexpm/v/brod_oauth.svg)](https://hex.pm/packages/brod_oauth)
[![Docs](https://img.shields.io/badge/hex-docs-green.svg?style=flat)](https://hexdocs.pm/brod_oauth)
[![Erlang Versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-25.0%20to%2027.0-blue)](http://www.erlang.org)

`brod_oauth` is a plugin for [brod](https://github.com/kafka4beam/brod) which adds SASL/OAUTHBEARER auth support.

## Setup

Add `brod_oauth` to your deps in rebar.config or mix.exs :

### rebar.config

```erlang
{deps, [{brod_oauth, "0.1.0"}]}.
```

### mix.exs

```elixir
{:brod_oauth, "~> 0.1.0"}
```

**NOTE** : `brod_oauth` requires `kafka_protocol` >= `4.1.8`.

### Configuration and usage

The only required configuration for `brod` is a sasl callback tuple with specific configuration
for `brod_oauth` enclosed in a map. See [brod authentication
support](https://github.com/kafka4beam/brod?tab=readme-ov-file#authentication-support)
for more general information on authentication support in brod.


#### brod_oauth configuration map

**Required keys** :

- `token_callback` : An arity 1 callback function. Said function is given a map containing
connection properties and expected to return a map containing the key
`token`, pointing to a valid JWT retrieved from an oauth provider.
The current properties passed to the callback are as follows :
     - `client_id` : The client id of the brod client instance.
     - `host` : Hostname for the current connection requiring auth.
     - `timeout` : The timeout value associated with connection (i.e., `connect_timeout`).

Optional keys :

- `authz_id` : A utf-8 binary authz id value.
- `extensions` : A map of key value pairs, most commonly a logical cluster id and identity pool id.

#### Examples

Note : While the examples below demonstrate providing `extensions` in configuration, it is not a
required parameter, and not all setups require extensions to be provided. Such is the case with
`authz_id` as well.

##### Erlang

```erlang
-module(example).

-export([start_client/0, fetch_token/1]).

fetch_token(#{client_id := _, host := _, timeout := _) ->

    BodyParams = [
        {"grant_type", "grant_type"},
        {"client_secret", "client_secret"},
        {"client_id", "client_id"},
        {"scope", "scope"}
    ],

    Body = uri_string:compose_query(BodyParams1),

    Res = httpc:request(
        post,
        {"https://my.oauth.provider", [], "application/x-www-form-urlencoded", Body},
        [],
        [{body_format, binary}]
    ),

    case Res of
        {ok, {{"HTTP/1.1", 200, "OK"}, _headers, Json}} ->
            #{<<"access_token">> := Token} = json:decode(Json),
            {ok, #{
                token => Token
            }};
        _Err ->
            {error, failed_to_obtain_jwt}
    end.

start_client() ->
  BootstrapEndpoints = [{"my.kafka.broker.host", 9092}],
  Extensions = #{
                  <<"logicalCluster">> => <<"lkc-1234">>,
                  <<"identityPoolId">> => <<"pool-4321">>},
  BrodOauthConfig = #{token_callback => fun example:fetch_token/1, extensions => Extensions},
  BrodConfig = [{connect_timeout, 60000},
                {sasl, {callback, brod_oauth, BrodOauthConfig}}],
  ok = brod:start_client(BootstrapEndpoints, my_client, BrodConfig).
```

##### Elixir

```elixir

defmodule Example do

  def oauth_params(%{client_id: _, host: _,  timeout: _) do
    body_params = %{
      "grant_type" => "grant_type",
      "client_secret" => "client_secret",
      "client_id" => "client_id",
      "scope" => "scope"
    }

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
        %{"access_token" => token} = :json.decode(json)

        {:ok,
         %{
           token: token,
           extensions: extensions
         }}

      _ ->
        {:error, :failed_to_obtain_jwt}
    end
  end

  def start_client() do
    bootstrap_endpoints = [{"my.kafka.broker.host", 9092}]
    extensions = %{"logicalCluster" => "lkc-1234", "identityPoolId" => "pool-4321"}
    brod_oauth_config = %{token_callback: &Example:fetch_token/1, extensions: extensions}
    brod_config = [connect_timeout: 60000, sasl: {:callback, :brod_oauth, brod_oauth_config}}]
    :ok = :brod.start_client(bootstrap_endpoints, :my_client, brod_config)
  end
```

See the [examples](https://github.com/HCA-Healthcare/brod_oauth/tree/main/examples) for more detail.
Make sure to set `KC_KAFKA_BROKER_SECRET` env variable before running examples test. Its value can be any text

# Further reading

- [Kafka KIP-255](https://cwiki.apache.org/confluence/pages/viewpage.action?pageId=75968876)
- [Kafka KIP-768](https://cwiki.apache.org/confluence/pages/viewpage.action?pageId=186877575)
- [RFC-5801](https://www.rfc-editor.org/rfc/rfc5801.html)
