#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"

sh "$THIS_DIR/shell.sh" --mini --github \
   "--run './nix/release-native.sh all'"
