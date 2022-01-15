#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
ROOT_DIR="$THIS_DIR/.."

mkdir -p $ROOT_DIR/build

for x in $ROOT_DIR/dhall/docker-compose.*.dhall; do

  [ -f "$x" ] || (echo "FILE_DOES_NOT_EXIST $x" && exit 1)

  FILE_NAME_DHALL=$(basename -- "$x")
  FILE_NAME="${FILE_NAME_DHALL%.dhall}"
  FILE_NAME_YAML="$FILE_NAME.yml"
  SRC_DHALL="$ROOT_DIR/dhall/$FILE_NAME_DHALL"

  if [ -f "$SRC_DHALL" ]; then
    RESULT_YAML="$ROOT_DIR/build/$FILE_NAME_YAML"
    dhall-to-yaml \
      --file "$SRC_DHALL" \
      --output "$RESULT_YAML" \
      --generated-comment
  fi

done

sh -c "$THIS_DIR/dhall-lint.sh"
