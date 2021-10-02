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
      buy (ExchangeRate 'Bfx.Buy)
      sell (ExchangeRate 'Bfx.Sell)
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
      price (ExchangeRate 'Bfx.Buy)
      gain (MoneyAmount 'Bfx.Base)
      loss (MoneyAmount 'Bfx.Quote)
      fee (FeeRate 'Bfx.Base)
      status OrderStatus
      insertedAt UTCTime
      deriving Eq Show

    CounterOrder
      intRef OrderId
      extRef (OrderExternalId 'Bfx.Sell) Maybe
      price (ExchangeRate 'Bfx.Sell)
      gain (MoneyAmount 'Bfx.Quote)
      loss (MoneyAmount 'Bfx.Base)
      fee (FeeRate 'Bfx.Quote)
      status OrderStatus
      insertedAt UTCTime
      deriving Eq Show

  |]
