{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Class.Env
  ( Env (..),
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Data.Time
import RecklessTradingBot.Import.External

class MonadIO m => Env m where
  withBfx :: (Bfx.Env -> m a) -> m a
  getPairs :: m (Set Bfx.CurrencyPair)
  getProfit :: m Bfx.ProfitRate
  getOrderTtl :: m Seconds
  sleepPriceTtl :: m ()
