#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"

sh \
  "$THIS_DIR/../reckless-trading-bot/nix/shutdown-test-deps.sh" \
  || true

build () {
  nix-build "$THIS_DIR/default.nix" \
    -A "$2" \
    -v --show-trace \
    --out-link "result-$1"
}

build_bitfinex_client () {
  build \
    bitfinex-client \
    bitfinex-client.components.library
}

build_reckless_trading_bot () {
  build \
    reckless-trading-bot \
    reckless-trading-bot.components.exes.reckless-trading-bot-exe
  nix-build "$THIS_DIR/../reckless-trading-bot/nix/docker.nix" \
    -v --show-trace \
    --out-link "result-docker-image-reckless-trading-bot"
}

TARGET="$1"
case $1 in
  bitfinex-client)
    build_bitfinex_client
    shift
    ;;
  reckless-trading-bot)
    build_reckless_trading_bot
    shift
    ;;
  all)
    build_bitfinex_client
    build_reckless_trading_bot
    shift
    ;;
  *)
    echo "==> unrecognized target $1"
    exit 1
    ;;
esac
