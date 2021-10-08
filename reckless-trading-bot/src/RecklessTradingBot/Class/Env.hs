{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Class.Env
  ( Env (..),
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Class.Storage
import qualified RecklessTradingBot.Data.Env as EnvData
import RecklessTradingBot.Data.Model
import RecklessTradingBot.Data.Time
import RecklessTradingBot.Import.External

class (Storage m, KatipContext m) => Env m where
  withBfx :: (Bfx.Env -> m a) -> m a
  getPairs :: m [EnvData.TradingConf]
  getProfit :: m Bfx.ProfitRate
  getOrderTtl :: m Seconds
  putCurrPrice :: Entity Price -> m ()
  rcvNextPrice :: m (Entity Price)
  sleepTillNextPrice :: Bfx.CurrencyPair -> m ()
