{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Kind
  ( Method (..),
    CurrencyRelation (..),
    MarketRelation (..),
    Location (..),
  )
where

data Method
  = SymbolsDetails
  | MarketAveragePrice
  | FeeSummary
  | Wallets
  | SubmitOrder
  | RetrieveOrders
  | OrdersHistory
  | CancelOrderMulti

data CurrencyRelation
  = Base
  | Quote

data MarketRelation
  = Maker
  | Taker

data Location
  = Local
  | Remote
