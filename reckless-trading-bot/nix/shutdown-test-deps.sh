#!/bin/sh

set -m

THIS_DIR="$(dirname "$(realpath "$0")")"
PGDATA="$THIS_DIR/../postgres"

timeout 5 pg_ctl -D $PGDATA stop
