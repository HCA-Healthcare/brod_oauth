#!/bin/bash -e

rm -rf _checkouts
mkdir -p _checkouts/brod_oauth

for f in $(find ../../ -type d -not -path "../../" | grep -v -E "git|build|example")
do
    cp -af $f _checkouts/brod_oauth
done

rebar3 escriptize

./_build/default/bin/example
