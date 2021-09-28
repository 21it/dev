{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.ToRequestMethod
  ( ToRequestMethod (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Web

class ToRequestMethod (method :: Method) where
  toRequestMethod :: RequestMethod

instance ToRequestMethod 'MarketAveragePrice where
  toRequestMethod = POST

instance ToRequestMethod 'FeeSummary where
  toRequestMethod = POST

instance ToRequestMethod 'SubmitOrder where
  toRequestMethod = POST

instance ToRequestMethod 'RetrieveOrders where
  toRequestMethod = POST

instance ToRequestMethod 'OrdersHistory where
  toRequestMethod = POST

instance ToRequestMethod 'CancelOrderMulti where
  toRequestMethod = POST
