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

       data CurrencyRelation
         = Base
         | Quote

       data MarketRelation
         = Maker
         | Taker

       data Location
         = Local
         | Remote

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

deriving stock instance Generic ExchangeAction

newExchangeAction ::
  Rational ->
  Either
    (TryFromException Rational ExchangeAction)
    ExchangeAction
newExchangeAction x
  | x > 0 = Right Buy
  | x < 0 = Right Sell
  | otherwise = Left $ TryFromException x Nothing
