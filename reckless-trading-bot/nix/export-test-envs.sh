#!/bin/sh

export RECKLESS_TRADING_BOT_ENV="
{
  \"bfx\":{
    \"api_key\":\"$BITFINEX_API_KEY\",
    \"prv_key\":\"$BITFINEX_PRV_KEY\"
  },
  \"tele\":{
    \"key\":\"$TELEGRAM_BOT_KEY\",
    \"chat\":\"$TELEGRAM_CHAT_ID\"
  },
  \"order_ttl\":3600,
  \"report_start_amt\":\"0.00830573\",
  \"report_currency\":\"BTC\",
  \"base_blacklist\":[\"LEO\"],
  \"libpq_conn_str\":\"postgresql://$USER@localhost/reckless-trading-bot\",
  \"log_env\":\"21it\",
  \"log_format\":\"Bracket\",
  \"log_severity\":\"info\",
  \"log_verbosity\":\"V3\"
}
"
