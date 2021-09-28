{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Kind
  ( Method (..),
    CurrencyRelation (..),
    MarketRelation (..),
    Location (..),
  )
where

import BitfinexClient.Import.External

data Method
  = MarketAveragePrice
  | FeeSummary
  | SubmitOrder
  | RetrieveOrders
  | OrdersHistory
  | CancelOrderMulti
  deriving stock (Show)

data CurrencyRelation
  = Base
  | Quote

data MarketRelation
  = Maker
  | Taker

data Location
  = Local
  | Remote
