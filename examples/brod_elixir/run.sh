#!/bin/bash -e

mix deps.get
mix compile
mix example
