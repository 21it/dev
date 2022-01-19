#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"

sh "$THIS_DIR/shell.sh" --mini \
   "--run './nix/release-native.sh reckless-trading-bot-docker'"
