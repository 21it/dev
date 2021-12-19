{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Kind where

import BitfinexClient.Import.External
import Data.Singletons.Base.TH

$( singletons
     [d|
       data Method
         = SymbolsDetails
         | MarketAveragePrice
         | FeeSummary
         | Wallets
         | SubmitOrder
         | RetrieveOrders
         | OrdersHistory
         | CancelOrderMulti
         deriving stock
           ( Eq,
             Ord,
             Show,
             Enum,
             Bounded
           )

       data CurrencyKind
         = Crypto
         | Stable
         | Fiat
         deriving stock
           ( Eq,
             Ord,
             Show,
             Enum,
             Bounded
           )

       data CurrencyRelation
         = Base
         | Quote
         deriving stock
           ( Eq,
             Ord,
             Show,
             Enum,
             Bounded
           )

       data MarketRelation
         = Maker
         | Taker
         deriving stock
           ( Eq,
             Ord,
             Show,
             Enum,
             Bounded
           )

       data Location
         = Local
         | Remote
         deriving stock
           ( Eq,
             Ord,
             Show,
             Enum,
             Bounded
           )

       data ExchangeAction
         = Buy
         | Sell
         deriving stock
           ( Eq,
             Ord,
             Show,
             Enum,
             Bounded
           )
       |]
 )

deriving stock instance Generic Method

deriving stock instance Generic CurrencyKind

deriving stock instance Generic CurrencyRelation

deriving stock instance Generic MarketRelation

deriving stock instance Generic Location

deriving stock instance Generic ExchangeAction

instance ToJSON Method

instance ToJSON CurrencyKind

instance ToJSON CurrencyRelation

instance ToJSON MarketRelation

instance ToJSON Location

instance ToJSON ExchangeAction

instance FromJSON Method

instance FromJSON CurrencyKind

instance FromJSON CurrencyRelation

instance FromJSON MarketRelation

instance FromJSON Location

instance FromJSON ExchangeAction
