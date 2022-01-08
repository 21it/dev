#!/bin/sh

set -m

#
# INFO : nix postgres don't like long sockets paths
#

THIS_DIR="$(dirname "$(realpath "$0")")"
PGDATA="$THIS_DIR/../postgres"
SOCKET_DIRECTORIES=`mktemp -d`
initdb -D $PGDATA --auth=trust --no-locale --encoding=UTF8
pg_ctl start \
  -D $PGDATA \
  -l $PGDATA/log.txt \
  -o "-c listen_addresses=localhost \
      -c unix_socket_directories=$SOCKET_DIRECTORIES"

until sh -c "pg_isready -h localhost" 2>/dev/null; do
  >&2 echo "postgres is unavailable - sleeping for 3 seconds..."
  sleep 3
done

createdb -h localhost reckless-trading-bot
createdb -h localhost reckless-trading-bot-test
