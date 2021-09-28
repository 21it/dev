#!/bin/sh

#
# app
#

export RECKLESS_TRADING_BOT_LOG_ENV="dev"
export RECKLESS_TRADING_BOT_LOG_FORMAT="Bracket" # Bracket | JSON
export RECKLESS_TRADING_BOT_LOG_VERBOSITY="V3"
export RECKLESS_TRADING_BOT_LIBPQ_CONN_STR="postgresql://nixbld1@localhost/reckless-trading-bot"
export RECKLESS_TRADING_BOT_ENDPOINT_PORT="3000"
export RECKLESS_TRADING_BOT_PRICE_TTL="300"
