#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
STACK="$1"

down() {
  docker stack rm "$1"

  until [ -z "$(docker stack ps "$1" -q)" ]
  do
    echo "$1 ==> stopping..."
    sleep 1
  done

  echo "$1 ==> stopped"
}

if [ -z "$STACK" ]
then
  for STACK in 21it; do
    down $STACK
  done
else
  down $STACK
fi
