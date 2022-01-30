#!/bin/sh

set -e

docker exec -it \
  `docker ps -f name=$1 --quiet` \
  sh
