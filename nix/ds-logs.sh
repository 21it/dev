#!/bin/sh

set -e

#
# Fix for the bug
# https://github.com/moby/moby/issues/33673
#


case $1 in
  psql)
    SERVICE="21it_postgres"
    shift
    ;;
  bot)
    SERVICE="21it_reckless-trading-bot"
    shift
    ;;
  *)
    SERVICE="21it_reckless-trading-bot"
    break
    ;;
esac

#docker service logs --timestamps "$SERVICE" | sort -k 1
#docker service logs --timestamps --follow --tail 0 "$SERVICE"

docker logs `docker ps -f name=$SERVICE --quiet` -f --tail 100
