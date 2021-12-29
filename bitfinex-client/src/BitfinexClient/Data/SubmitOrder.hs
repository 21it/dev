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

data Request (act :: ExchangeAction) = Request
  { amount :: MoneyAmt 'Base act,
    symbol :: CurrencyPair,
    rate :: QuotePerBase act,
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

instance
  ( ToRequestParam (MoneyAmt 'Base act),
    ToRequestParam (QuotePerBase act),
    SingI act
  ) =>
  ToJSON (Request act)
  where
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
            A..= toTextParam (amount req),
          "symbol"
            A..= toTextParam (symbol req),
          "price"
            A..= toTextParam (rate req),
          "flags"
            A..= unOrderFlagSet (flags opts)
        ]
    where
      opts = options req
