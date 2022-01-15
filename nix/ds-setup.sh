#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
BUILD_DIR="$THIS_DIR/../build"

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

echo "==> docker swarm network setup"
sh "$THIS_DIR/ds-down.sh" || true
docker swarm leave --force || true
docker swarm init || true
docker network create -d overlay --attachable global || true

echo "==> docker image build"
sh "$THIS_DIR/release-docker.sh"
docker load -q -i \
  "$BUILD_DIR/docker-image-reckless-trading-bot.tar.gz" \
  | awk '{print $NF}' \
  | tr -d '\n' \
  > "$BUILD_DIR/docker-image-reckless-trading-bot.txt"

echo "==> dhall compilation"
sh "$THIS_DIR/shell.sh" --mini \
   "--run './nix/dhall-compile.sh'"

sh "$THIS_DIR/ds-up.sh"
