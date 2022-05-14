#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
BUILD_DIR="$THIS_DIR/../build"

if [ -z "$1" ]; then
  echo "==> Github release/package tag is missing! Failing..."
  exit 1
else
  VERSION="$1"
fi

for APP in reckless-trading-bot; do
  OLD_TAG=`cat "$BUILD_DIR/docker-image-$APP.txt"`
  NEW_TAG="ghcr.io/21it/$APP:$VERSION"
  echo "$APP ==> Tag $OLD_TAG -> $NEW_TAG"
  docker tag "$OLD_TAG" "$NEW_TAG"
  echo "$APP ==> Push $NEW_TAG"
  docker push "$NEW_TAG"
done
