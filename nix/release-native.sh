#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
ROOT_DIR="$THIS_DIR/.."
BUILD_DIR="$ROOT_DIR/build"

sh \
  "$ROOT_DIR/reckless-trading-bot/nix/shutdown-test-deps.sh" \
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


build_reckless_trading_bot_native () {
  nix-bundle "(import $THIS_DIR/default.nix).reckless-trading-bot.components.exes.reckless-trading-bot-exe" "/bin/reckless-trading-bot-exe"
  mv "$ROOT_DIR/reckless-trading-bot-exe" \
     "$BUILD_DIR/reckless-trading-bot"
  #
  # NOTE : simple dynamically linked build
  # is not that useful because of many shared libraries
  #
  # build \
  #   reckless-trading-bot \
  #   reckless-trading-bot.components.exes.reckless-trading-bot-exe
  # rm -rf \
  #   "$BUILD_DIR/reckless-trading-bot"
  # cp -Lr \
  #   "$ROOT_DIR/result-reckless-trading-bot/bin/reckless-trading-bot-exe" \
  #   "$BUILD_DIR/reckless-trading-bot"
  #
  # NOTE : for linker debug
  #
  # ldd "$BUILD_DIR/reckless-trading-bot"
  # objdump -j .interp -s "$BUILD_DIR/reckless-trading-bot"
  #
  # NOTE : for fixing shared libraries linking
  #
  # chmod +rwx \
  #   "$BUILD_DIR/reckless-trading-bot"
  # patchelf --set-interpreter \
  #   /lib64/ld-linux-x86-64.so.2 "$BUILD_DIR/reckless-trading-bot"
}

build_reckless_trading_bot_docker () {
  nix-build "$ROOT_DIR/reckless-trading-bot/nix/docker.nix" \
    -v --show-trace \
    --out-link "result-docker-image-reckless-trading-bot"
  rm -rf \
    "$BUILD_DIR/docker-image-reckless-trading-bot.tar.gz"
  cp -Lr \
    "$ROOT_DIR/result-docker-image-reckless-trading-bot" \
    "$BUILD_DIR/docker-image-reckless-trading-bot.tar.gz"
}

TARGET="$1"
case $1 in
  bitfinex-client)
    build_bitfinex_client
    shift
    ;;
  reckless-trading-bot-native)
    build_reckless_trading_bot_native
    shift
    ;;
  reckless-trading-bot-docker)
    build_reckless_trading_bot_docker
    shift
    ;;
  all)
    build_bitfinex_client
    build_reckless_trading_bot_native
    build_reckless_trading_bot_docker
    shift
    ;;
  *)
    echo "==> unrecognized target $1"
    exit 1
    ;;
esac
