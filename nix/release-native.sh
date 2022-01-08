#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"

sh \
  "$THIS_DIR/../reckless-trading-bot/nix/shutdown-test-deps.sh" \
  || true

build () {
  nix-build "$THIS_DIR/../$1/nix/$2.nix" \
    -v --show-trace \
    --out-link "result-$1-$2"
}

build bitfinex-client default
build reckless-trading-bot default
build reckless-trading-bot docker
