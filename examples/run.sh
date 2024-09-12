#!/bin/bash -e

docker-compose down
docker-compose up -d


echo ""
echo "Waiting for kafka and keycloak to become ready..."
KAFKA_READY=1

set +e
while [ $KAFKA_READY -ne 0 ]
do
    docker logs kafka-oauth-test | grep -q "Kafka Server started"
    KAFKA_READY=$?
done
echo "done."

set -e

echo ""
echo "Running tests from Erlang..."
cd brod_erlang && ./run.sh
cd ..
echo ""
echo "Running tests from Elixir..."
cd brod_elixir && ./run.sh
cd ..
echo ""
echo "Done."
docker-compose down
