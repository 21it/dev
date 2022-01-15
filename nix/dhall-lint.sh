#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
ROOT_DIR="$THIS_DIR/.."

for x in $(find $ROOT_DIR/dhall/ -type f -name "*.dhall"); do
  dhall lint "$x"
  dhall format "$x"
done
