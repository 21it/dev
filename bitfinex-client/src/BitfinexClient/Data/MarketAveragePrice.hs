{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.MarketAveragePrice
  ( Request (..),
  )
where

import BitfinexClient.Import

data Request (act :: ExchangeAction) = Request
  { amount :: MoneyAmt 'Base act,
    symbol :: CurrencyPair
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
