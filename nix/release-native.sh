#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"

sh \
  "$THIS_DIR/../reckless-trading-bot/nix/shutdown-test-deps.sh" \
  || true

build () {
  nix-build "$THIS_DIR/default.nix" \
    -A "$1" \
    -v --show-trace \
    --out-link "result-$1"
}

build \
  bitfinex-client.components.library

build \
  reckless-trading-bot.components.exes.reckless-trading-bot-exe

nix-build "$THIS_DIR/../reckless-trading-bot/nix/docker.nix" \
  -v --show-trace \
  --out-link "result-docker-image-reckless-trading-bot"
