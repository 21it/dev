let P = ./Prelude/Import.dhall

let J = P.JSON

let mempty = [] : P.Map.Type Text Text

in  { networks.global.external = True
    , version = "3"
    , volumes.postgres = mempty
    , services =
      { postgres =
        { image = "heathmont/postgres:11-alpine-a2e8bbe"
        , hostname = "postgres"
        , environment =
          { POSTGRES_MULTIPLE_DATABASES = "\"reckless-trading-bot\""
          , POSTGRES_PASSWORD = "user"
          , POSTGRES_USER = "user"
          }
        , volumes = [ "postgres:/var/lib/postgresql/data" ]
        , networks.global = mempty
        }
      , reckless-trading-bot =
        { image = ../build/docker-image-reckless-trading-bot.txt as Text
        , hostname = "reckless-trading-bot"
        , environment.RECKLESS_TRADING_BOT_ENV
          =
            J.render
              ( J.object
                  ( toMap
                      { bfx =
                          J.object
                            ( toMap
                                { api_key =
                                    J.string
                                      "\${BITFINEX_API_KEY:?BITFINEX_API_KEY}"
                                , prv_key =
                                    J.string
                                      "\${BITFINEX_PRV_KEY:?BITFINEX_PRV_KEY}"
                                }
                            )
                      , tele =
                          J.object
                            ( toMap
                                { key =
                                    J.string
                                      "\${TELEGRAM_BOT_KEY:?TELEGRAM_BOT_KEY}"
                                , chat =
                                    J.string
                                      "\${TELEGRAM_CHAT_ID:?TELEGRAM_CHAT_ID}"
                                }
                            )
                      , order_ttl = J.natural 3600
                      , report_start_amt = J.string "0.00830573"
                      , report_currency = J.string "BTC"
                      , libpq_conn_str =
                          J.string
                            "postgresql://user:user@postgres/reckless-trading-bot"
                      , log_env = J.string "21it"
                      , log_format = J.string "Bracket"
                      , log_severity = J.string "info"
                      , log_verbosity = J.string "V3"
                      }
                  )
              )
        , networks.global = mempty
        }
      }
    }
