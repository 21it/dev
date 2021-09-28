{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.MarketAveragePrice
  ( Request (..),
  )
where

import BitfinexClient.Import

data Request = Request
  { action :: ExchangeAction,
    amount :: MoneyAmount,
    symbol :: CurrencyPair
  }
  deriving stock (Eq, Ord, Show)
