{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.SubmitOrder
  ( Request (..),
    Options (..),
    optsDef,
    optsPostOnly,
  )
where

import BitfinexClient.Import
import qualified Data.Aeson as A

data Request = Request
  { action :: ExchangeAction,
    amount :: MoneyAmount,
    symbol :: CurrencyPair,
    rate :: ExchangeRate,
    options :: Options
  }
  deriving stock (Eq, Ord, Show)

data Options = Options
  { clientId :: Maybe OrderClientId,
    groupId :: Maybe OrderGroupId,
    flags :: Set OrderFlag
  }
  deriving stock (Eq, Ord, Show)

optsDef :: Options
optsDef =
  Options
    { clientId = Nothing,
      groupId = Nothing,
      flags = mempty
    }

optsPostOnly :: Options
optsPostOnly =
  Options
    { clientId = Nothing,
      groupId = Nothing,
      flags = [PostOnly]
    }

instance ToJSON Request where
  toJSON req =
    eradicateNull $
      A.object
        [ "gid"
            A..= groupId opts,
          "cid"
            A..= clientId opts,
          "type"
            A..= ("EXCHANGE LIMIT" :: Text),
          "amount"
            A..= toTextParam (action req, amount req),
          "symbol"
            A..= toTextParam (symbol req),
          "price"
            A..= toTextParam (rate req),
          "flags"
            A..= unOrderFlagSet (flags opts)
        ]
    where
      opts = options req
