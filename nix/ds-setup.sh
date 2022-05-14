#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
BUILD_DIR="$THIS_DIR/../build"
SETUP_MODE="prebuilt"
RESET_SWARM="false"
GITHUB_RELEASE="v0.1.1"

if [ -z "$*" ]; then
  echo "==> using defaults"
else
  for arg in "$@"; do
    case $arg in
      --source)
        SETUP_MODE="source"
        shift
        ;;
      --prebuilt)
        SETUP_MODE="prebuilt"
        shift
        ;;
      --reset-swarm)
        RESET_SWARM="true"
        shift
        ;;
      *)
        echo "==> unrecognized arg $arg"
        exit 1
        ;;
    esac
  done
fi

echo "==> docker swarm network setup"
sh "$THIS_DIR/ds-down.sh" || true
if [ "$RESET_SWARM" = "true" ]; then
  echo "==> DOING SWARM RESET"
  docker swarm leave --force || true
  docker swarm init || true
else
  echo "==> using existing swarm"
fi
docker network create -d overlay --attachable global || true

echo "==> cleanup build dir"
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

if [ "$SETUP_MODE" = "source" ]; then
  (
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
  )
else
  (
    echo "==> download prebuilt release"
    cd "$BUILD_DIR"
    wget "https://github.com/21it/src/releases/download/$GITHUB_RELEASE/docker-image-reckless-trading-bot.tar.gz"
    wget "https://github.com/21it/src/releases/download/$GITHUB_RELEASE/docker-compose.21it.yml"
    docker load -q -i \
      "$BUILD_DIR/docker-image-reckless-trading-bot.tar.gz"
  )
fi

sh "$THIS_DIR/ds-up.sh"
