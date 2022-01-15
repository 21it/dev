#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"

rm -rf "$THIS_DIR/../build"
mkdir -p "$THIS_DIR/../build"

sh "$THIS_DIR/shell.sh" --mini \
   "--run './nix/release-native.sh reckless-trading-bot && \
   cp -Lr \
     ./result-docker-image-reckless-trading-bot \
     ./build/docker-image-reckless-trading-bot.tar.gz && \
   cp -Lr \
     ./result-reckless-trading-bot/bin/reckless-trading-bot-exe \
     ./build/reckless-trading-bot'
   "
