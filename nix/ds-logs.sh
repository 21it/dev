#!/bin/sh

set -e

#
# Fix for the bug
# https://github.com/moby/moby/issues/33673
#

SERVICE="21it_reckless-trading-bot"

#docker service logs --timestamps "$SERVICE" | sort -k 1
#docker service logs --timestamps --follow --tail 0 "$SERVICE"

docker logs `docker ps -f name=$SERVICE --quiet` -f --tail 100
