#!/bin/sh

set -e

docker exec -it \
  `docker ps -f name=21it_postgres --quiet` \
  psql -U user reckless-trading-bot
