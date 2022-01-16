#!/bin/sh

#
# app
#

export RECKLESS_TRADING_BOT_PAIRS="
{
  \"ADABTC\":{
    \"currency_kind\":\"Crypto\",
    \"max_quote_investment\":\"0.04\",
    \"min_profit_per_order\":\"0.001\"
  },
  \"XMRBTC\":{
    \"currency_kind\":\"Crypto\",
    \"max_quote_investment\":\"0.01\",
    \"min_profit_per_order\":\"0.001\"
  }
}
"
export RECKLESS_TRADING_BOT_PRICE_TTL="300"
export RECKLESS_TRADING_BOT_ORDER_TTL="900"

export RECKLESS_TRADING_BOT_LOG_ENV="dev"
export RECKLESS_TRADING_BOT_LOG_FORMAT="Bracket"
export RECKLESS_TRADING_BOT_LOG_SEVERITY="DebugS"
export RECKLESS_TRADING_BOT_LOG_VERBOSITY="V3"
export RECKLESS_TRADING_BOT_LIBPQ_CONN_STR="postgresql://nixbld1@localhost/reckless-trading-bot"
