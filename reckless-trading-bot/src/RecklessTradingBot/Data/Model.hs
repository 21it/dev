{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Model where

import qualified BitfinexClient as Bfx
import Database.Persist.TH
import RecklessTradingBot.Data.Money
import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External
import RecklessTradingBot.Orphan ()

--
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
--
-- Syntax for models:
-- https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md
--
share
  [mkPersist sqlSettings, mkMigrate "migrateAuto"]
  [persistLowerCase|

    Price
      base (CurrencyCode 'Bfx.Base)
      quote (CurrencyCode 'Bfx.Quote)
      buy (QuotePerBase 'Bfx.Buy)
      sell (QuotePerBase 'Bfx.Sell)
      at UTCTime
      deriving Eq Show

    Order
      --
      -- Price which triggered the Order
      --
      priceRef PriceId
      --
      -- Store CurrencyPair just in case
      -- if Order will not correspond to
      -- single price in the future
      --
      base (CurrencyCode 'Bfx.Base)
      quote (CurrencyCode 'Bfx.Quote)
      --
      -- Order might refer to another Order
      -- as follow-up in some strategies,
      -- for example Martingale.
      --
      intRef OrderId Maybe
      extRef (OrderExternalId 'Bfx.Buy) Maybe
      price (QuotePerBase 'Bfx.Buy)
      gain (MoneyBase 'Bfx.Buy)
      loss (MoneyQuote 'Bfx.Buy)
      fee (FeeRate 'Bfx.Maker 'Bfx.Base)
      status OrderStatus
      at UTCTime
      deriving Eq Show

    CounterOrder
      intRef OrderId
      extRef (OrderExternalId 'Bfx.Sell) Maybe
      price (QuotePerBase 'Bfx.Sell)
      gain (MoneyQuote 'Bfx.Sell)
      loss (MoneyBase 'Bfx.Sell)
      fee (FeeRate 'Bfx.Maker 'Bfx.Quote)
      status OrderStatus
      at UTCTime
      deriving Eq Show

  |]

instance TryFrom OrderId Bfx.OrderClientId where
  tryFrom =
    Bfx.OrderClientId `composeTryRhs` tryFrom
