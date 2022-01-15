let P = ./Prelude/Import.dhall

in  { networks.global.external = True
    , version = "3"
    , volumes.postgres = [] : P.Map.Type Text Text
    , services.postgres
      =
      { image = "heathmont/postgres:11-alpine-a2e8bbe"
      , hostname = "postgres"
      , environment =
        { POSTGRES_MULTIPLE_DATABASES = "\"reckless-trading-bot\""
        , POSTGRES_PASSWORD = "user"
        , POSTGRES_USER = "user"
        }
      , volumes = [ "postgres:/var/lib/postgresql/data" ]
      , networks.global = [] : P.Map.Type Text Text
      }
    , services.reckless-trading-bot
      =
      { image = ../build/docker-image-reckless-trading-bot.txt as Text
      , hostname = "reckless-trading-bot"
      , environment =
        { BITFINEX_API_KEY = "\${BITFINEX_API_KEY:?BITFINEX_API_KEY}"
        , BITFINEX_PRV_KEY = "\${BITFINEX_PRV_KEY:?BITFINEX_PRV_KEY}"
        , RECKLESS_TRADING_BOT_PAIRS =
            ''
            {
              "ADABTC":{
                "currency_kind":"Crypto",
                "max_quote_investment":"0.04",
                "min_profit_per_order":"0.001"
              },
              "XMRBTC":{
                "currency_kind":"Crypto",
                "max_quote_investment":"0.01",
                "min_profit_per_order":"0.001"
              }
            }
            ''
        , RECKLESS_TRADING_BOT_PRICE_TTL = "300"
        , RECKLESS_TRADING_BOT_ORDER_TTL = "900"
        , RECKLESS_TRADING_BOT_LOG_ENV = "21it"
        , RECKLESS_TRADING_BOT_LOG_FORMAT = "Bracket"
        , RECKLESS_TRADING_BOT_LOG_VERBOSITY = "V3"
        , RECKLESS_TRADING_BOT_LIBPQ_CONN_STR =
            "postgresql://user:user@postgres/reckless-trading-bot"
        }
      , networks.global = [] : P.Map.Type Text Text
      }
    }
