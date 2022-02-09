#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
BUILD_DIR="$THIS_DIR/../build"

echo "==> Binaries build"
sh "$THIS_DIR/shell.sh" --mini --github \
   "--run './nix/release-native.sh all'"

echo "==> Chown"
sudo chown -R $USER:$USER "$BUILD_DIR"

echo "==> Docker image verification"
docker load -q -i \
  "$BUILD_DIR/docker-image-reckless-trading-bot.tar.gz" \
  | awk '{print $NF}' \
  | tr -d '\n' \
  > "$BUILD_DIR/docker-image-reckless-trading-bot.txt"

echo "==> Dhall compilation"
sh "$THIS_DIR/shell.sh" --mini --github \
   "--run './nix/dhall-compile.sh'"

echo "==> Chown"
sudo chown -R $USER:$USER "$BUILD_DIR"

echo "==> DEBUG ls "
ls -la "$BUILD_DIR"
echo "==> DEBUG image name"
cat "$BUILD_DIR/docker-image-reckless-trading-bot.txt"
echo "==> DEBUG swarm file"
cat "$BUILD_DIR/docker-compose.21it.yml"
