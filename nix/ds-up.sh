#!/bin/sh

THIS_DIR="$(dirname "$(realpath "$0")")"

echo "==> swarm file verification"
docker-compose \
  -f "$THIS_DIR/../build/docker-compose.21it.yml" \
  config 1>/dev/null

echo "==> swarm file deploy"
docker stack deploy \
  --with-registry-auth \
  -c "$THIS_DIR/../build/docker-compose.21it.yml" 21it
