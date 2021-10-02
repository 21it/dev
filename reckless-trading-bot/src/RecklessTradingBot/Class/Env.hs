{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Class.Env
  ( Env (..),
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Data.Model
import RecklessTradingBot.Data.Time
import RecklessTradingBot.Import.External

class Storage m => Env m where
  withBfx :: (Bfx.Env -> m a) -> m a
  getPairs :: m (Set Bfx.CurrencyPair)
  getProfit :: m Bfx.ProfitRate
  getOrderTtl :: m Seconds
  putCurrPrice :: Price -> m ()
  rcvNextPrice :: m Price
  sleepTillNextPrice :: Bfx.CurrencyPair -> m ()
