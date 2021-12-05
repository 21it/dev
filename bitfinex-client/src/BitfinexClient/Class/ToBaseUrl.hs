{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.ToBaseUrl
  ( ToBaseUrl (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Web

class ToBaseUrl (method :: Method) where
  toBaseUrl :: BaseUrl

instance ToBaseUrl 'SymbolsDetails where
  toBaseUrl = base

instance ToBaseUrl 'MarketAveragePrice where
  toBaseUrl = base

instance ToBaseUrl 'FeeSummary where
  toBaseUrl = base

instance ToBaseUrl 'SubmitOrder where
  toBaseUrl = base

instance ToBaseUrl 'RetrieveOrders where
  toBaseUrl = base

instance ToBaseUrl 'OrdersHistory where
  toBaseUrl = base

instance ToBaseUrl 'CancelOrderMulti where
  toBaseUrl = base

instance ToBaseUrl 'Wallets where
  toBaseUrl = base

base :: BaseUrl
base = "https://api.bitfinex.com"
